create_clock -name clk -period 10.0 [get_ports clock]


set_input_delay -max 9.0 -clock clk [get_ports {ports_*_ingress_valid}]
set_input_delay -max 9.0 -clock clk [get_ports {ports_*_ingress_bits_*}]
set_input_delay -max 6.0 -clock clk [get_ports {ports_*_egress_ready}]

set_output_delay -max 3.0 -clock clk [get_ports {ports_*_egress_valid}]
set_output_delay -max 3.0 -clock clk [get_ports {ports_*_egress_bits_*}]
set_output_delay -max 8.0 -clock clk [get_ports {ports_*_ingress_ready}]

