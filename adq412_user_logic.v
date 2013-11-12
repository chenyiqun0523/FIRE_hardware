//////////////////////////////////////////////////////////////////////////////////
//
// Copyright  2012 Signal Processing Devices Sweden AB. All rights reserved.
//
// Module Name:    user_logic
// Project Name:   ADQ
//
// Revision:
// Description:    Module for ADQ412 DevKit
//
/////////////////////////////////////////////////////////////////////////////////
`include "dft_top_12_scale.v"
module adq412_user_logic
  #(
    parameter ChanWidth = 96,
    parameter FullDataWidth = 128
    )
  (
   
   // Clocks and reset
    input wire                           clk_1_8,        // 1:8 sample clock ratio clock
    input wire                           clk_1_4,        // 1:4 sample clock ratio clock
    input wire                           clk50,          // 50MHz clock
    input wire                           clk_regs,       // 75MHz register clock
    input wire                           rst_i,          // Reset signal
   
   // Trig signals
    output reg [15:0] 		         ext_trig_vector_o,
    input wire [15:0]                    ext_trig_vector_i,
    output wire                          host_trig_o,
    input wire                           host_trig_i,

   // Data input
    input wire [ChanWidth-1:0]           data_a_i,
    input wire [ChanWidth-1:0]           data_b_i,
    input wire [ChanWidth-1:0]           data_c_i,
    input wire [ChanWidth-1:0]           data_d_i,
    input wire                           data_valid_i,
   
   // Data output
    output wire [FullDataWidth-1:0]      data_a_o,
    output wire [FullDataWidth-1:0]      data_b_o,
    output wire [FullDataWidth-1:0]      data_c_o,
    output wire [FullDataWidth-1:0]      data_d_o,
    output wire                          data_valid_o,
   
   // User registers
    input wire [32*16-1:0]               user_register_i,
    output wire [32*16-1:0]              user_register_o,
   
   // GPIO port HW side
    input wire [3:0]                     com_gpio_i,
    output wire [3:0]                    com_gpio_o,
    output wire [3:0]                    com_gpio_oen_o,
   
   // GPIO port processor side
    output wire [3:0]                    com_gpio_mcu_o,
    input wire [3:0]                     com_gpio_mcu_i,
    input wire [3:0]                     com_gpio_oen_mcu_i,

   // Trigger output HW side
    input wire                           trigout_i,         //This signal is from the physical trigout port going back into the FPGA when the trigout port is used as an input port
    output wire                          trigout_o,         //This signal is the trigout signal GOING TO the physical trigout port
    output wire                          trigout_oen_o,     // This signal is the enable signal to the physical trigout port
   
   // Trigger output processor side
    output wire                          trigout_mcu_o,       //This signal carries the trigout signal INTO the FPGA when the trigout port is used as an input port
    input wire                           trigout_mcu_i,       //This signal is a trigout signal generated FROM the FPGA
    input wire                           trigout_oen_mcu_i,    //This signal is an enable signal FROM the FPGA to enable the physical trigout port,
   
    output wire [31:0]                   ul_partnumber_1_o,
    output wire [31:0]                   ul_partnumber_2_o
   );
   
      
   // -----------------------------------------------------------------------------------------------
   // This section sets the user logic part number, which can be set in the user logic build script
   // using set_userlogicpartnumber and read out through the API using GetUserLogicPartNumber().
   // Either rebuild the project or modify the include file, in order to change part number.
   `include "userlogicpartnumber.v"
   assign ul_partnumber_1_o = {`USER_LOGIC_PARTNUM_2 , `USER_LOGIC_PARTNUM_1};    
   assign ul_partnumber_2_o = {`USER_LOGIC_PARTNUM_REV , `USER_LOGIC_PARTNUM_3};
   // -----------------------------------------------------------------------------------------------
   

   /* Placement comment:
      User logic should be placed in AREA_GROUP = "pblock_user_logic"
      to reach timing more easily. Don't move the input and output registers
      Data is clocked in and out of the user_logic with clk_1_8.
    */ 
   reg [31:0] user_register_in [16-1:0];
   reg [31:0] user_register_out [16-1:0];

   // *** SIGNALS ****
   
   (* SHREG_EXTRACT="NO" *) (* ASYNC_REG="TRUE" *) reg [31:0] user_register_capture [15:0];
   (* SHREG_EXTRACT="NO" *)  reg [31:0] user_register_sync [15:0];
   
   /* Placement comment: a and b input registers are located on lower left of device */
   /* Placement comment: c and d input registers are located on upper mid of device */
   (* SHREG_EXTRACT = "NO" *) (* KEEP = "TRUE" *) reg [FullDataWidth-1:0] data_a_input;
   (* SHREG_EXTRACT = "NO" *) (* KEEP = "TRUE" *) reg [FullDataWidth-1:0] data_b_input;
   (* SHREG_EXTRACT = "NO" *) (* KEEP = "TRUE" *) reg [FullDataWidth-1:0] data_c_input;
   (* SHREG_EXTRACT = "NO" *) (* KEEP = "TRUE" *) reg [FullDataWidth-1:0] data_d_input;
   (* SHREG_EXTRACT = "NO" *) (* KEEP = "TRUE" *) reg data_valid_input;
   
   (* SHREG_EXTRACT = "NO" *) (* KEEP = "TRUE" *) reg [FullDataWidth-1:0] data_a_mid;
   (* SHREG_EXTRACT = "NO" *) (* KEEP = "TRUE" *) reg [FullDataWidth-1:0] data_b_mid;
   (* SHREG_EXTRACT = "NO" *) (* KEEP = "TRUE" *) reg [FullDataWidth-1:0] data_c_mid;
   (* SHREG_EXTRACT = "NO" *) (* KEEP = "TRUE" *) reg [FullDataWidth-1:0] data_d_mid;
   (* SHREG_EXTRACT = "NO" *) (* KEEP = "TRUE" *) reg data_valid_mid;

   (* SHREG_EXTRACT = "NO" *) (* KEEP = "TRUE" *) reg [FullDataWidth-1:0] data_a_output;
   (* SHREG_EXTRACT = "NO" *) (* KEEP = "TRUE" *) reg [FullDataWidth-1:0] data_b_output;
   (* SHREG_EXTRACT = "NO" *) (* KEEP = "TRUE" *) reg [FullDataWidth-1:0] data_c_output;
   (* SHREG_EXTRACT = "NO" *) (* KEEP = "TRUE" *) reg [FullDataWidth-1:0] data_d_output;
   (* SHREG_EXTRACT = "NO" *) (* KEEP = "TRUE" *) reg data_valid_output;
   
   // Ext trigger
   reg [15:0]                            ext_trig_vector_d1;
   reg [15:0]                            ext_trig_vector_d2;
   reg [15:0]                            ext_trig_det_rising_edge;
   reg                                   ext_trig;
   
   wire                                 data_valid_stream;
   reg [31:0]                           datacounter;
   reg [31:0]                           timecounter;

   // *** LOGIC ****
   reg [6:0] seven_bit_counter;		//for continuous streaming
   
   wire next;
   reg [11:0] xn0_re,xn1_re,xn2_re,xn3_re,xn4_re,xn5_re,xn6_re,xn7_re;		//input real
   wire [11:0] xn0_im,xn1_im,xn2_im,xn3_im,xn4_im,xn5_im,xn6_im,xn7_im;	//input image
   
   
   wire next_out;	//output from dft

   wire [11:0] xk0_re,xk1_re,xk2_re,xk3_re,xk4_re,xk5_re,xk6_re,xk7_re;	//output real
   wire [11:0] xk0_im,xk1_im,xk2_im,xk3_im,xk4_im,xk5_im,xk6_im,xk7_im;	//output image
   
   
   dft_top mydft(.clk(clk_1_8), .reset(rst_i), .next(next), .next_out(next_out),
    .X0(xn0_re), .Y0(xk0_re),
    .X1(xn0_im), .Y1(xk0_im),
    .X2(xn1_re), .Y2(xk1_re),
    .X3(xn1_im), .Y3(xk1_im),
    .X4(xn2_re), .Y4(xk2_re),
    .X5(xn2_im), .Y5(xk2_im),
    .X6(xn3_re), .Y6(xk3_re),
    .X7(xn3_im), .Y7(xk3_im),
    .X8(xn4_re), .Y8(xk4_re),
    .X9(xn4_im), .Y9(xk4_im),
    .X10(xn5_re), .Y10(xk5_re),
    .X11(xn5_im), .Y11(xk5_im),
    .X12(xn6_re), .Y12(xk6_re),
    .X13(xn6_im), .Y13(xk6_im),
    .X14(xn7_re), .Y14(xk7_re),
    .X15(xn7_im), .Y15(xk7_im)
    );

   assign xn0_im = 12'b0;
   assign xn1_im = 12'b0;
   assign xn2_im = 12'b0;
   assign xn3_im = 12'b0;
   assign xn4_im = 12'b0;
   assign xn5_im = 12'b0;
   assign xn6_im = 12'b0;
   assign xn7_im = 12'b0;
 
 
//	cordic translation block getting magnitude of complex number
	wire [11:0] x0_out,x1_out,x2_out,x3_out,x4_out,x5_out,x6_out,x7_out;	//ouptut of translation
	
   	cordic_v4_0 trans0(                         
	.x_in(xk0_re), // Bus [11 : 0] 
	.y_in(xk0_im), // Bus [11 : 0] 
	.x_out(x0_out), // Bus [11 : 0]
	.clk(clk_1_8));
	
	cordic_v4_0 trans1(
	.x_in(xk1_re), // Bus [11 : 0] 
	.y_in(xk1_im), // Bus [11 : 0] 
	.x_out(x1_out), // Bus [11 : 0]
	.clk(clk_1_8)); // Bus [11 : 0] 
	
	cordic_v4_0 trans2(
	.x_in(xk2_re), // Bus [11 : 0] 
	.y_in(xk2_im), // Bus [11 : 0] 
	.x_out(x2_out), // Bus [11 : 0]
	.clk(clk_1_8)); // Bus [11 : 0] 
	
	cordic_v4_0 trans3(
	.x_in(xk3_re), // Bus [11 : 0] 
	.y_in(xk3_im), // Bus [11 : 0] 
	.x_out(x3_out), // Bus [11 : 0]
	.clk(clk_1_8)); // Bus [11 : 0] 
	
	cordic_v4_0 trans4(
	.x_in(xk4_re), // Bus [11 : 0] 
	.y_in(xk4_im), // Bus [11 : 0] 
	.x_out(x4_out), // Bus [11 : 0]
	.clk(clk_1_8)); // Bus [11 : 0] 
	
	cordic_v4_0 trans5(
	.x_in(xk5_re), // Bus [11 : 0] 
	.y_in(xk5_im), // Bus [11 : 0] 
	.x_out(x5_out), // Bus [11 : 0]
	.clk(clk_1_8)); // Bus [11 : 0] 
	
	cordic_v4_0 trans6(
	.x_in(xk6_re), // Bus [11 : 0] 
	.y_in(xk6_im), // Bus [11 : 0] 
	.x_out(x6_out), // Bus [11 : 0]
	.clk(clk_1_8)); // Bus [11 : 0] 
	
	cordic_v4_0 trans7(
	.x_in(xk7_re), // Bus [11 : 0] 
	.y_in(xk7_im), // Bus [11 : 0] 
	.x_out(x7_out), // Bus [11 : 0]
	.clk(clk_1_8)); // Bus [11 : 0] 
	
	reg [15:0] next_out_r;     //16 shift registers to delay next_out signal by 16 clock cycles 
	reg [6:0] seven_bit_counter_2;	//for cutting off half data
	
	
	//256 samples buffer for cell detection
	reg [11:0] window_buffer [511:0];	
	
	
	//50*512 samples 12-bit pre buffer using block RAM
	reg [95:0] pre_in_r;
	wire [95:0] pre_out_w;
	reg pre_clock_enable_r;
	reg pre_write_enable_r;
	reg [11:0] pre_address_r;
	
	blk_mem_gen_v4_3 pre_buffer (
	.clka(clk_1_8),
	.wea(pre_write_enable_r), // Bus [0 : 0] 
	.addra(pre_address_r), // Bus [11 : 0] 
	.dina(pre_in_r), // Bus [95 : 0] 
	.douta(pre_out_w)); // Bus [95 : 0] 
	
	//50*512 samples 12-bit post buffer using block RAM
	reg [95:0] post_in_r;
	wire [95:0] post_out_w;
	reg post_clock_enable_r;
	reg post_write_enable_r;
	reg [11:0] post_address_r;
	
	blk_mem_gen_v4_3 post_buffer (
	.clka(clk_1_8),
	.wea(post_write_enable_r), // Bus [0 : 0] 
	.addra(post_address_r), // Bus [11 : 0] 
	.dina(post_buffer_in_r), // Bus [95 : 0] 
	.douta(post_buffer_out_w)); // Bus [95 : 0] 
	
	// reserved for cell detection 
	reg cell_detected_r;
	reg [14:0] pre_address_detect_r; //for storing address of pre buffer when cell is detected
	
	//data output
	reg [ChanWidth-1:0] data_a_output_r;
	reg data_valid_r;
	
	//reg finish_push_pre;	//flag of finishing pushing all data in pre buffer
	//reg finish_push_post;	//flag of finishing pushing all data in post buffer
	reg [11:0] counter3200;	//3200 cycle counter to counter cycles of pre&post buffer push out 50*512/8=3200
	
	//FSM
	reg [2:0] state;
	reg [2:0] next_state;
	
	
	always@(posedge clk_1_8)
	begin
		if (rst_i == 1'b1)			
			state <= 3'b000;;		
		else
			state <= next_state;
	end	
	
	always@(*)
	begin
		case(state)
		3'b000:begin
				if(next_out_r[15] == 1) begin
					next_state = 3'b001;
				end else begin
					next_state = 3'b000;
				end
			end
		3'b001:begin
				if (seven_bit_counter_2[6] == 1) begin
					next_state = 3'b010;
				end else begin
					next_state = 3'b001;
				end
			end
		3'b010: begin
				if ((seven_bit_counter_2[6] == 0)&&(cell_detected_r == 0)) begin
					next_state = 3'b001;
				end else if ((seven_bit_counter_2[6] == 0)&&(cell_detected_r == 1)) begin
					next_state = 3'b011;
				end else begin
					next_state = 3'b010;
				end
			end
		3'b011: begin
				if(seven_bit_counter_2[6] == 1) begin
					next_state = 3'b100;
				end else begin
					next_state = 3'b011;
				end
			end
		3'b100: begin
				if((seven_bit_counter_2[6] == 0)&&(counter3200 == 12'd3200)) begin
					next_state = 3'b101;
				end else if ((seven_bit_counter_2[6] == 0)&&(counter3200 ~= 12'd3200)) begin
					next_state = 3'b011;
				end else begin
					next_state = 3'b100;
				end
			end
		3'b101: begin
				if(seven_bit_counter_2[6] == 1) begin
					next_state = 3'b110;
				end else begin
					next_state = 3'b101;
				end
			end
		3'b110: begin
				if(seven_bit_counter_2[6] == 0) begin
					if(counter3200 ~= 12'd3200) begin
						next_state = 3'b101;
					end else begin
						if(cell_detected_r == 0) begin
							next_state = 3'b001;
						end else begin
							next_state = 3'b011;	
						end
					end
				end else begin
					next_state = 3'b110;
				end
			end
		endcase
	end
	
	always@(negedge clk_1_8) begin			//still deciding pos or neg edge of clock
		if(rst_i) begin
			cell_detected_r <= 0;
			seven_bit_counter_2 <= 7'b0000000;
			pre_write_enable_r <= 0;
			pre_address_r <= 12'd3199;		//initial to 25599 so that first write address will be 0
			pre_in_r <= 96'h000000000000000000000000;	
			window_buffer <= 0;
			
			post_write_enable_r <= 0;
			post_address_r <= 12'd3199;		//initial to 25599 so that first write address will be 0
			post_in_r <= 96'h000000000000000000000000;	
			
			data_a_output_r <= 96'h000000000000000000000000;
			data_valid_r <= 0;
			pre_address_detect_r <= 12'b000000000000000;
			
			//finish_push_pre <= 0;
			//finish_push_post <= 0;
			counter3200 <= 12'd0;
		end 
		else begin
			case(next_state)
			3'b000: begin
				
			end
			3'b001: begin
				seven_bit_counter_2 <= seven_bit_counter_2 + 1;
				pre_write_enable_r <= 1;
				if(pre_address_r == 12'd3199) begin
					pre_address_r <= 12'b0;
				end else begin
					pre_address_r <= pre_address_r + 1;
				end
				pre_in_r <= {x7_out,x6_out,x5_out,x4_out,x3_out,x2_out,x1_out,x0_out};
				window_buffer[511:504] <= {x7_out,x6_out,x5_out,x4_out,x3_out,x2_out,x1_out,x0_out};
				window_buffer[503:0] <= window_buffer[511:8];		//in window buffer #511 is latest sample high frequency, #0 is oldest sample low frequency
			end 
			3'b010: begin
				seven_bit_counter_2 <= seven_bit_counter_2 + 1;
				pre_write_enable_r <= 0;	//turn off write enable of pre buffer
				//construct cell detection logic here
				
				cell_detectd_r <= 0;	
				
				pre_address_detect_r <= pre_address_r;
			end
			3'b011: begin
				cell_detectd_r <= 0; //reset cell detect flag back to 0
				seven_bit_counter_2 <= seven_bit_counter_2 + 1;
				post_write_enable_r <= 1;
				if(post_address_r == 12'd3199) begin
					post_address_r <= 12'b0;
				end else begin
					post_address_r <= post_address_r + 1;
				end
				post_in_r <= {x7_out,x6_out,x5_out,x4_out,x3_out,x2_out,x1_out,x0_out};
				
				pre_write_enable_r <= 0;
				if(pre_address_r == 12'd3199) begin
					pre_address_r <= 12'b0;
				end else begin
					pre_address_r <= pre_address_r + 1;
				end
				data_a_output_r <= pre_out_w;
				data_valid_r <= 1'b1;
				if(counter3200 == 12'd3200) begin	//reset 3200 counter 
					counter3200 <= 12'd0;
				end
			end
			3'b100: begin
				seven_bit_counter_2 <= seven_bit_counter_2 + 1;
				post_write_enable_r <= 0;

				counter3200 <= counter3200 + 1;
			end
			3'b101: begin
				seven_bit_counter_2 <= seven_bit_counter_2 + 1;
				if(post_address_r == 12'd3199) begin
					post_address_r <= 12'b0;
				end else begin
					post_address_r <= post_address_r + 1;
				end
				data_a_output_r <= post_out_w;
				data_valid_r <= 1'b1;
				
				pre_write_enable_r <= 1;
				pre_address_r <= pre_address_r + 1;
				pre_in_r <= {x7_out,x6_out,x5_out,x4_out,x3_out,x2_out,x1_out,x0_out};
				window_buffer[511:504] <= {x7_out,x6_out,x5_out,x4_out,x3_out,x2_out,x1_out,x0_out};
				window_buffer[503:0] <= window_buffer[511:8];
				
				if(counter3200 == 12'd3200) begin	//reset 3200 counter 
					counter3200 <= 12'd0;
				end
			end
			3'b110: begin
				seven_bit_counter_2 <= seven_bit_counter_2 + 1;
				pre_write_enable_r <= 0;
				counter3200 <= counter3200 + 1;
				
				//construct cell detection logic here
				
				cell_detectd_r <= 0;
			end
			endcase
		end
	end
	
	always @(posedge clk_1_8) begin
		if(rst_i) begin 
			seven_bit_counter <= 7'b0000000;
			next_out_r <= 16'h0000;
		end
		else begin
			seven_bit_counter <= seven_bit_counter + 1;	//continuous incrementing
			next_out_r <= {next_out_r[15:0],next_out};	//delay 16 cycles to compensate delay of translation IP	
		end
	end
	assign next = (seven_bit_counter==7'b0000001)?1:0;  //for continuous streaming, raise next signal every 128 clock cycles
	
   // Control Registers logic

   genvar i;
   generate
      for (i=0; i < 16; i=i+1) 
        begin: user_in
           always @(posedge clk_1_8) begin
              user_register_capture[i] <= user_register_i[32*(i+1)-1:32*i];
              user_register_sync[i] <= user_register_capture[i];
              user_register_in[i] <= user_register_sync[i];
           end
        end
   endgenerate

   assign user_register_o = {user_register_out[15],
                             user_register_out[14],
                             user_register_out[13],
                             user_register_out[12],
                             user_register_out[11],
                             user_register_out[10],
                             user_register_out[9],
                             user_register_out[8],
                             user_register_out[7],
                             user_register_out[6],
                             user_register_out[5],
                             user_register_out[4],
                             user_register_out[3],
                             user_register_out[2],
                             user_register_out[1],
                             user_register_out[0]};

   // Mux for data path
   always @(posedge clk_1_8) begin
      user_register_out[1] <= user_register_in[1];
      if (user_register_in[1][1])
        begin
           // Streaming mode: 16 bit unpacked data
           data_a_input <= {{16'h0007},{16'h0006},{16'h0005},{16'h0004}, timecounter[31:16],timecounter[15:0],datacounter[31:16],datacounter[15:0]};
           data_b_input <= {{16'h000f},{16'h000e},{16'h000d},{16'h000c}, {16'h000b},{16'h000a},{16'h0009},{16'h0008}};
           data_c_input <= {{16'h0017},{16'h0016},{16'h0015},{16'h0014}, {16'h0013},{16'h0012},{16'h0011},{16'h0010}};
           data_d_input <= {{16'h001f},{16'h001e},{16'h001d},{16'h001c}, {16'h001b},{16'h001a},{16'h0019},{16'h0018}};
           data_valid_input <= data_valid_stream;  //Remember to turn on external trigger mode from API, otherwise it will not work!!!

        end

      else if (user_register_in[1][2])
        begin
           // Normal mode 12-bit packed data
           data_a_input <= {{12'h007},{12'h006},{12'h005},{12'h004}, {12'h003},{12'h002},{12'h001},{12'h000}};
           data_b_input <= {{12'h00f},{12'h00e},{12'h00d},{12'h00c}, {12'h00b},{12'h00a},{12'h009},{12'h008}};
           data_c_input <= {{12'h017},{12'h016},{12'h015},{12'h014}, {12'h013},{12'h012},{12'h011},{12'h010}};
           data_d_input <= {{12'h01f},{12'h01e},{12'h01d},{12'h01c}, {12'h01b},{12'h01a},{12'h019},{12'h018}};
        end 

      else 
        begin
           // Normal mode 12-bit packed data
           data_a_input <= data_a_i[ChanWidth-1:0];
           data_b_input <= data_b_i[ChanWidth-1:0];
           data_c_input <= data_c_i[ChanWidth-1:0];
           data_d_input <= data_d_i[ChanWidth-1:0];
           data_valid_input <= data_valid_i;

        end

      if  (data_valid_i == 1'b1)
        begin
          timecounter <= timecounter+1;
        end
        
     if  ((data_valid_stream == 1'b1) & (user_register_in[1][1] == 1'b1))
        datacounter <= datacounter + 1;
     else if (user_register_in[1][1] == 1'b0)
        datacounter <= 0;            
      
   end
   
   assign data_valid_stream = (user_register_in[1][3] == 1'b1) ? ext_trig : trigout_mcu_i_sync;
   
   // Pass through these signals when not used in user logic

   // GPIO
   assign com_gpio_mcu_o = com_gpio_i;
   assign com_gpio_o = com_gpio_mcu_i;
   assign com_gpio_oen_o = com_gpio_oen_mcu_i;

   // Host trig
   assign host_trig_o = host_trig_i;
   
   (* SHREG_EXTRACT = "NO" *) reg  trigout_i_sync;
   (* SHREG_EXTRACT = "NO" *) reg  trigout_mcu_i_sync;
   (* SHREG_EXTRACT = "NO" *) reg  trigout_oen_mcu_i_sync;
 
   (* SHREG_EXTRACT = "NO" *) reg  trigout_i_sync_d1;
   (* SHREG_EXTRACT = "NO" *) reg  trigout_mcu_i_sync_d1;
   (* SHREG_EXTRACT = "NO" *) reg  trigout_oen_mcu_i_sync_d1;
    
   always @(posedge clk_1_8) begin
      trigout_i_sync <= trigout_i;
      trigout_mcu_i_sync <= trigout_mcu_i;
      trigout_oen_mcu_i_sync <= trigout_oen_mcu_i;

      trigout_i_sync_d1 <= trigout_i_sync;
      trigout_mcu_i_sync_d1 <= trigout_mcu_i_sync;
      trigout_oen_mcu_i_sync_d1 <= trigout_oen_mcu_i_sync;
   end 
   
   // Trigout port
   assign trigout_mcu_o = trigout_i_sync_d1;
   assign trigout_o = trigout_mcu_i_sync_d1;
   assign trigout_oen_o = trigout_oen_mcu_i_sync_d1;

   assign data_a_o = data_a_output;
   assign data_b_o = data_b_output;
   assign data_c_o = data_c_output;
   assign data_d_o = data_d_output;
   assign data_valid_o = data_valid_output;
   
   // Ext trig logic
   always @(posedge clk_1_8) begin
      ext_trig_vector_d1 <= ext_trig_vector_i;
      ext_trig_vector_d2 <= ext_trig_vector_d1;
      ext_trig_vector_o <= ext_trig_vector_d2;
      
      ext_trig_det_rising_edge <= ~{ext_trig_vector_i[14:0], ext_trig_vector_d1[15]} & ext_trig_vector_i;
      ext_trig <= {|ext_trig_det_rising_edge};
   end

   always @(posedge clk_1_8)
     begin
        xn0_re <= data_a_input[11:0];
		xn1_re <= data_a_input[23:12];
		xn2_re <= data_a_input[35:24];
		xn3_re <= data_a_input[47:36];
		xn4_re <= data_a_input[59:48];
		xn5_re <= data_a_input[71:60];
		xn6_re <= data_a_input[83:72];
		xn7_re <= data_a_input[95:84];
	
        data_b_mid <= data_b_input;
        data_c_mid <= data_c_input;
        data_d_mid <= data_d_input;
        data_valid_mid <= data_valid_input;
        
		data_a_output <= data_a_output_r;
        data_b_output <= data_b_mid;
        data_c_output <= data_c_mid;
        data_d_output <= data_d_mid;
		data_valid_output <= data_valid_r;
end
     

   
endmodule

/*module seven_bit_counter_2(clk,rst,start,count_out);
	input clk;
	input rst;
	input start;
	output reg [6:0] count_out;
	
	
	always@(posedge clk) begin
		if(rst) begin
			count_out <= 7'b1111111;
		end
		else begin
			if((count_out == 7'b1111111 && ~start)||(count_out ~= 7'b1111111 && start))
				count_out <= count_out + 1;
			else 
				count_out <= count_out;
		end
	end

endmodule*/
