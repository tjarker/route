create_clock -name clk -period 10.0 [get_ports clock]


set_input_delay -max 0 -clock clk [get_ports {*}]
set_output_delay -max 0 -clock clk [get_ports {*}]


