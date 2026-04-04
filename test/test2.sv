module nested_loop (
  input wire [7:0] data_in [8][8],
  output reg       data_out
);

  always_comb begin
    data_out = 0;
    for (int i=0; i<8; i=i+1) begin
      for (int j=0; j<8; j=j+1) begin
        for (int k=0; k<8; k=k+1) begin
          data_out = data_out ^ data_in[i][j][k];
        end
      end
    end

  end

endmodule
