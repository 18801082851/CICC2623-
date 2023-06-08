//****************************************Copyright (c)***********************************//
//原子哥在线教学平台：www.yuanzige.com
//技术支持：www.openedv.com
//淘宝店铺：http://openedv.taobao.com
//关注微信公众平台微信号："正点原子"，免费获取ZYNQ & FPGA & STM32 & LINUX资料。
//版权所有，盗版必究。
//Copyright(C) 正点原子 2018-2028
//All rights reserved
//----------------------------------------------------------------------------------------
// File name:           hdmi_top
// Last modified Date:  2020/05/04 9:19:08
// Last Version:        V1.0
// Descriptions:        HDMI显示顶层模块
//                      
//----------------------------------------------------------------------------------------
// Created by:          正点原子
// Created date:        2019/05/04 9:19:08
// Version:             V1.0
// Descriptions:        The original version
//
//----------------------------------------------------------------------------------------
//****************************************************************************************//

module  hdmi_top(
    input           pixel_clk,
    input           pixel_clk_5x,    
    input           sys_rst_n,
   //hdmi接口
   // input           hpdin,    
    output          tmds_clk_p,     // TMDS 时钟通道
    output          tmds_clk_n,
    output [2:0]    tmds_data_p,    // TMDS 数据通道
    output [2:0]    tmds_data_n,
 //   output          tmds_oen ,      // TMDS 输出使能
 //   output          hpdout,
   
    //xuyaoxinhao
   // input   [15:0]  video_rgb_565,
    input           video_hs_i  ,
    input           video_vs_i  ,
    input           video_de_i  ,
   
   //用户接口 
    output          video_vs,       //HDMI场信号  
    output          video_hs, 
    output          video_de,   
    output  [10:0]  h_disp,         //HDMI屏水平分辨率
    output  [10:0]  v_disp,         //HDMI屏垂直分辨率     
    output  [10:0]  pixel_xpos,     //像素点横坐标
    output  [10:0]  pixel_ypos,     //像素点纵坐标        
    input   [15:0]  data_in,        //输入数据
    output   [10:0]  cnt_h,
    output   [10:0]  cnt_v,
    output          data_req        //请求数据输入   
);

//wire define
wire          pixel_clk;
wire          pixel_clk_5x;
wire          clk_locked;
wire  [2:0]   tmds_data_p;   
wire  [2:0]   tmds_data_n;
wire  [10:0]  pixel_xpos;
wire  [10:0]  pixel_ypos;
wire  [10:0]  h_disp;
wire  [10:0]  v_disp;
wire          video_hs;
wire          video_vs;
wire          video_de;
wire  [23:0]  video_rgb_i;
wire  [23:0]  video_rgb_565;

//*****************************************************
//**                    main code
//*****************************************************

//assign hpdout = hpdin;
//将摄像头16bit数据转换为24bit的hdmi数据
assign video_rgb_i = {data_in[15:11],3'b000,data_in[10:5],2'b00,
                    data_in[4:0],3'b000};  

//例化视频显示驱动模块
video_driver u_video_driver(
    .pixel_clk      (pixel_clk),
    .sys_rst_n      (sys_rst_n),

    .video_hs       (video_hs),
    .video_vs       (video_vs),
    .video_de       (video_de),
   // .video_rgb      (video_rgb_565),
   
    .data_req       (data_req),
    .h_disp         (h_disp),
    .v_disp         (v_disp), 
    .pixel_xpos     (pixel_xpos),
    .pixel_ypos     (pixel_ypos),
    .cnt_h          (cnt_h),
    .cnt_v          (cnt_v),
    .pixel_data     (data_in)
    );
  wire [23:0] video_rgb_i_g;
  assign video_rgb_i_g = video_de_i ? video_rgb_i : 24'd0;     
//例化HDMI驱动模块
dvi_transmitter_top u_rgb2dvi_0(
    .pclk           (pixel_clk),
    .pclk_x5        (pixel_clk_5x),
    .reset_n        (sys_rst_n),
                
    .video_din      (video_rgb_i_g ),
    .video_hsync    (video_hs_i  ), 
    .video_vsync    (video_vs_i  ),
    .video_de       (video_de_i  ),
                
    .tmds_clk_p     (tmds_clk_p),
    .tmds_clk_n     (tmds_clk_n),
    .tmds_data_p    (tmds_data_p),
    .tmds_data_n    (tmds_data_n), 
     .tmds_oen       ()
    //.tmds_oen       (tmds_oen)
    );

endmodule 