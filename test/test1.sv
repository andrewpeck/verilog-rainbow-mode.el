module adder_tree
  #(int N=8,
    int W=1
) (
  input wire                   clk,
  input wire [W-1:0]           din [N],
  output reg [W+$clog2(N)-1:0] sum
);

  generate
    if (N==2) begin : gen_F

      always_ff @(posedge clk) begin
        sum <= din[0] + din[1];
      end

    end

    else begin: gen_T

      localparam int NPOW2 = 2**$clog2(N);
      localparam int NB = W+$clog2(N/2);

      logic [NB-1:0] sum_a ;
      logic [NB-1:0] sum_b ;

      logic [W-1:0]  din_a      [NPOW2/2];
      logic [W-1:0]  din_b      [NPOW2/2];
      logic [W-1:0]  din_padded [NPOW2];

      for (genvar i=0; i<NPOW2; i=i+1) begin : gen_din_padding
        if (i < N) begin : gen_din
          assign din_padded[i] = din[i];
        end else begin : gen_zero
          assign din_padded[i] = 0;
        end
      end

      for (genvar i=0; i<NPOW2/2; i=i+1) begin : gen_din_halves
        assign din_a[i] = din_padded[i];
        assign din_b[i] = din_padded[i + NPOW2/2];
      end

      adder_tree #(
        .N (NPOW2/2),
        .W (W))
      sub_tree_a (
        .clk (clk),
        .din (din_a),
        .sum (sum_a)
      );

      adder_tree #(
        .N (NPOW2/2),
        .W (W))
      sub_tree_b (
        .clk (clk),
        .din (din_b),
        .sum (sum_b)
      );

      always_ff @(posedge clk) begin
        sum <= $size(sum)'(sum_a) + $size(sum)'(sum_b);
      end

    end
  endgenerate
endmodule
