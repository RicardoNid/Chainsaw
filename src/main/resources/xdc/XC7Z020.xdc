create_clock -period 20.000 [get_ports clk]

# clocks
set_property PACKAGE_PIN Y9 [get_ports clk]
set_property IOSTANDARD LVCMOS33 [get_ports clk]

# keys
set_property PACKAGE_PIN B4 [get_ports {ps_key_n}]
set_property IOSTANDARD LVCMOS33 [get_ports {ps_key_n}]
set_property PACKAGE_PIN A17 [get_ports {pl_key_n}]
set_property IOSTANDARD LVCMOS18 [get_ports {pl_key_n}]

# 40pin-J15
set_property PACKAGE_PIN  T21  [get_ports {alinx40Pin1_N[0]} ]
set_property PACKAGE_PIN  U21  [get_ports {alinx40Pin1_P[0]} ]
set_property PACKAGE_PIN  U20  [get_ports {alinx40Pin1_N[1]} ]
set_property PACKAGE_PIN  V20  [get_ports {alinx40Pin1_P[1]} ]
set_property PACKAGE_PIN  Y19  [get_ports {alinx40Pin1_N[2]} ]
set_property PACKAGE_PIN  AA19  [get_ports {alinx40Pin1_P[2]} ]
set_property PACKAGE_PIN  J21  [get_ports {alinx40Pin1_N[3]} ]
set_property PACKAGE_PIN  J22  [get_ports {alinx40Pin1_P[3]} ]
set_property PACKAGE_PIN  K21  [get_ports {alinx40Pin1_N[4]} ]
set_property PACKAGE_PIN  J20  [get_ports {alinx40Pin1_P[4]} ]
set_property PACKAGE_PIN  P16  [get_ports {alinx40Pin1_N[5]} ]
set_property PACKAGE_PIN  R16  [get_ports {alinx40Pin1_P[5]} ]
set_property PACKAGE_PIN  M17  [get_ports {alinx40Pin1_N[6]} ]
set_property PACKAGE_PIN  L17  [get_ports {alinx40Pin1_P[6]} ]
set_property PACKAGE_PIN  T18  [get_ports {alinx40Pin1_N[7]} ]
set_property PACKAGE_PIN  R18  [get_ports {alinx40Pin1_P[7]} ]
set_property PACKAGE_PIN  P20  [get_ports {alinx40Pin1_N[8]} ]
set_property PACKAGE_PIN  P21  [get_ports {alinx40Pin1_P[8]} ]
set_property PACKAGE_PIN  T19  [get_ports {alinx40Pin1_N[9]} ]
set_property PACKAGE_PIN  R19  [get_ports {alinx40Pin1_P[9]} ]
set_property PACKAGE_PIN  P15  [get_ports {alinx40Pin1_N[10]} ]
set_property PACKAGE_PIN  N15  [get_ports {alinx40Pin1_P[10]} ]
set_property PACKAGE_PIN  M16  [get_ports {alinx40Pin1_N[11]} ]
set_property PACKAGE_PIN  M15  [get_ports {alinx40Pin1_P[11]} ]
set_property PACKAGE_PIN  AB19  [get_ports {alinx40Pin1_N[12]} ]
set_property PACKAGE_PIN  AB20  [get_ports {alinx40Pin1_P[12]} ]
set_property PACKAGE_PIN  W22  [get_ports {alinx40Pin1_N[13]} ]
set_property PACKAGE_PIN  V22  [get_ports {alinx40Pin1_P[13]} ]
set_property PACKAGE_PIN  W21  [get_ports {alinx40Pin1_N[14]} ]
set_property PACKAGE_PIN  W20  [get_ports {alinx40Pin1_P[14]} ]
set_property PACKAGE_PIN  AA21  [get_ports {alinx40Pin1_N[15]} ]
set_property PACKAGE_PIN  AB21  [get_ports {alinx40Pin1_P[15]} ]
set_property PACKAGE_PIN  Y21 [get_ports {alinx40Pin1_N[16]} ]
set_property PACKAGE_PIN  Y20 [get_ports {alinx40Pin1_P[16]} ]

# 40pin-J16


set_property IOSTANDARD LVCMOS18 [get_ports {alinx40Pin*}]