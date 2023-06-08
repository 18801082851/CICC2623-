
 `include "ysyx_22040210_define.v"
module ysyx_22040210_pht #(
	parameter GSH_PHT_NUM = 2**`ysyx_22040210_BHRLEN
			   )(
    input       wire                        clk,
    input       wire                        rst,
    input       wire  [`ysyx_22040210_BHR_BUS]          raddr1,
    //input       wire  [`ysyx_22040210_BHR_BUS]          raddr2,
    output      reg  [1:0]                      rdata1,
    //output      reg  [1:0]                      rdata2,
    input       wire           we,
    input       wire  [`ysyx_22040210_BHR_BUS]                      waddr,
    input       wire  [1:0]                      wdata
);

reg [1:0] pht [GSH_PHT_NUM -1 : 0] ;
integer i;

always @(posedge clk ) begin
    if (rst) begin
        for (i = 0; i < GSH_PHT_NUM; i = i + 1) begin
            pht[i] <= 2'b00;
        end
        rdata1 <= 2'b0;
        //rdata2 <= 2'b0;
    end else begin
        if (we == `ysyx_22040210_True) begin
                pht[waddr] <=  wdata;
        end 
        rdata1 <= pht[raddr1];
       // rdata2 <= pht[raddr2];
    end
end

endmodule
