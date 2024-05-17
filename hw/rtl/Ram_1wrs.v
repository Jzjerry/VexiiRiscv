module Ram_1wrs(
    input  clk   , //i
    input  en    , //i
    input  wr    , //i
    input  [13:0] addr , //i
    input  [3:0]  mask,//i
    input  [31:0] wrData , //i
    output [31:0] rdData , 
  );

  parameter wordCount = 16000;
  parameter wordWidth = 32;
  parameter readUnderWrite = "dontCare";
  parameter duringWrite = "dontCare";
  parameter technology = "auto";
  parameter maskWidth = 1;
  parameter maskEnable = 1'b0;

    // Bank A [31:16]
    SB_SPRAM256KA spram_A
    (
      .ADDRESS(addr),
      .DATAIN(wrData[31:16]),
      .MASKWREN({mask[3], mask[3], mask[2], mask[2]}),
      .WREN(wr),
      .CHIPSELECT(1'b1),
      .CLOCK(clk),
      .STANDBY(1'b0),
      .SLEEP(1'b0),
      .POWEROFF(1'b1),
      .DATAOUT(rdData[31:16])
    );

    // Bank B [15:0]
    SB_SPRAM256KA spram_B
    (
      .ADDRESS(addr),
      .DATAIN(wrData[15:0]),
      .MASKWREN({mask[1], mask[1], mask[0], mask[0]}),
      .WREN(wr),
      .CHIPSELECT(1'b1),
      .CLOCK(clk),
      .STANDBY(1'b0),
      .SLEEP(1'b0),
      .POWEROFF(1'b1),
      .DATAOUT(rdData[15:0])
    );

endmodule