module level_coefficients
contains
real function A(idx)
  integer   :: idx
  real, parameter, dimension(91)  ::  A_val=(/    0.0000,        2.0000,        3.9808,        7.3872,       12.9080, &
                                                 21.4140,       33.9530,       51.7470,       76.1680,      108.7200, &
                                                150.9900,      204.6400,      271.3600,      352.8200,      450.6900, &
                                                566.5200,      701.8100,      857.9500,     1036.2000,     1237.6000, &
                                               1463.2000,     1713.7000,     1989.9000,     2292.2000,     2620.9000, &
                                               2976.3000,     3358.4000,     3767.2000,     4202.4000,     4663.8000, &
                                               5150.9000,     5663.2000,     6199.8000,     6759.7000,     7341.5000, &
                                               7942.9000,     8564.6000,     9208.3000,     9873.6000,    10559.0000, &
                                              11262.0000,    11983.0000,    12714.0000,    13453.0000,    14192.0000, &
                                              14923.0000,    15638.0000,    16330.0000,    16991.0000,    17613.0000, &
                                              18191.0000,    18717.0000,    19185.0000,    19588.0000,    19920.0000, &
                                              20175.0000,    20349.0000,    20434.0000,    20426.0000,    20319.0000, &
                                              20107.0000,    19785.0000,    19349.0000,    18799.0000,    18141.0000, &
                                              17386.0000,    16545.0000,    15634.0000,    14666.0000,    13653.0000, &
                                              12608.0000,    11543.0000,    10471.0000,     9405.2000,     8356.3000, &
                                               7335.2000,     6353.9000,     5422.8000,     4550.2000,     3743.5000, &
                                               3010.1000,     2356.2000,     1784.9000,     1297.7000,      895.1900, &
                                                576.3100,      336.7700,      162.0400,       54.2080,        6.5756, &
                                                  0.31600E-02/)
  A = A_val(idx)
  return
end function A

real function B(idx)
  integer   :: idx
  real, parameter, dimension(91)  ::  B_val=(/0.0000,        0.0000,        0.0000,        0.0000,        0.0000,      &
                                              0.0000,        0.0000,        0.0000,        0.0000,        0.0000,      &
                                              0.0000,        0.0000,        0.0000,        0.0000,        0.0000,      &
                                              0.0000,        0.0000,        0.0000,        0.0000,        0.0000,      &
                                              0.0000,        0.0000,        0.0000,        0.0000,        0.0000,      &
                                              0.0000,        0.0000,        0.0000,        0.0000,        0.0000,      &
                                              0.0000,        0.0000,        0.0000,        0.0000,        0.27240E-06, &
                                              0.13912E-04,   0.54667E-04,   0.13136E-03,   0.27888E-03,   0.54838E-03, &
                                              0.10001E-02,   0.17011E-02,   0.27647E-02,   0.42670E-02,   0.63222E-02, &
                                              0.90350E-02,   0.12508E-01,   0.16860E-01,   0.22189E-01,   0.28610E-01, &
                                              0.36227E-01,   0.45146E-01,   0.55474E-01,   0.67316E-01,   0.80777E-01, &
                                              0.95964E-01,   0.11298,       0.13193,       0.15293,       0.17609,     &
                                              0.20152,       0.22931,       0.25955,       0.29199,       0.32633,     &
                                              0.36220,       0.39920,       0.43691,       0.47502,       0.51328,     &
                                              0.55146,       0.58932,       0.62656,       0.66293,       0.69822,     &
                                              0.73222,       0.76468,       0.79538,       0.82419,       0.85095,     &
                                              0.87552,       0.89777,       0.91765,       0.93516,       0.95027,     &
                                              0.96301,       0.97347,       0.98224,       0.98915,       0.99420,     &
                                              0.99763/)


  B = B_val(idx)
  return
end function B
end module level_coefficients
