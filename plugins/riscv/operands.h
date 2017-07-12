//
//  operands.h
//
//  DANGER - This is machine generated code
//

#ifndef rv_operands_h
#define rv_operands_h

typedef uimm_operand_t<5, S<11,7, B<4,0>>>                                         operand_rd;
typedef uimm_operand_t<5, S<19,15, B<4,0>>>                                        operand_rs1;
typedef uimm_operand_t<5, S<24,20, B<4,0>>>                                        operand_rs2;
typedef uimm_operand_t<5, S<31,27, B<4,0>>>                                        operand_rs3;
typedef uimm_operand_t<5, S<11,7, B<4,0>>>                                         operand_frd;
typedef uimm_operand_t<5, S<19,15, B<4,0>>>                                        operand_frs1;
typedef uimm_operand_t<5, S<24,20, B<4,0>>>                                        operand_frs2;
typedef uimm_operand_t<5, S<31,27, B<4,0>>>                                        operand_frs3;
typedef uimm_operand_t<1, S<26,26, B<0,0>>>                                        operand_aq;
typedef uimm_operand_t<1, S<25,25, B<0,0>>>                                        operand_rl;
typedef uimm_operand_t<4, S<27,24, B<3,0>>>                                        operand_pred;
typedef uimm_operand_t<4, S<23,20, B<3,0>>>                                        operand_succ;
typedef uimm_operand_t<3, S<14,12, B<2,0>>>                                        operand_rm;
typedef simm_operand_t<32, S<31,12, B<31,12>>>                                     operand_imm20;
typedef simm_operand_t<32, S<31,12, B<31,12>>>                                     operand_oimm20;
typedef simm_operand_t<21, S<31,12, B<20>,B<10,1>,B<11>,B<19,12>>>                 operand_jimm20;
typedef simm_operand_t<12, S<31,20, B<11,0>>>                                      operand_imm12;
typedef simm_operand_t<12, S<31,20, B<11,0>>>                                      operand_oimm12;
typedef uimm_operand_t<12, S<31,20, B<11,0>>>                                      operand_csr12;
typedef simm_operand_t<12, S<31,25, B<11,5>>, S<11,7, B<4,0>>>                     operand_simm12;
typedef simm_operand_t<13, S<31,25, B<12>,B<10,5>>, S<11,7, B<4,1>,B<11>>>         operand_sbimm12;
typedef uimm_operand_t<5, S<19,15, B<4,0>>>                                        operand_zimm;
typedef uimm_operand_t<5, S<24,20, B<4,0>>>                                        operand_shamt5;
typedef uimm_operand_t<6, S<25,20, B<5,0>>>                                        operand_shamt6;
typedef uimm_operand_t<7, S<26,20, B<6,0>>>                                        operand_shamt7;
typedef uimm_operand_t<1, S<12,12, B<0,0>>>                                        operand_crd0;
typedef uimm_operand_t<3, S<4,2, B<2,0>>>                                          operand_crdq;
typedef uimm_operand_t<3, S<9,7, B<2,0>>>                                          operand_crs1q;
typedef uimm_operand_t<3, S<9,7, B<2,0>>>                                          operand_crs1rdq;
typedef uimm_operand_t<3, S<4,2, B<2,0>>>                                          operand_crs2q;
typedef uimm_operand_t<5, S<11,7, B<4,0>>>                                         operand_crd;
typedef uimm_operand_t<5, S<11,7, B<4,0>>>                                         operand_crs1;
typedef uimm_operand_t<5, S<11,7, B<4,0>>>                                         operand_crs1rd;
typedef uimm_operand_t<5, S<6,2, B<4,0>>>                                          operand_crs2;
typedef uimm_operand_t<3, S<4,2, B<2,0>>>                                          operand_cfrdq;
typedef uimm_operand_t<3, S<4,2, B<2,0>>>                                          operand_cfrs2q;
typedef uimm_operand_t<5, S<6,2, B<4,0>>>                                          operand_cfrs2;
typedef uimm_operand_t<5, S<11,7, B<4,0>>>                                         operand_cfrd;
typedef uimm_operand_t<5, S<6,2, B<4,0>>>                                          operand_cimmsh5;
typedef uimm_operand_t<6, S<12,12, B<5>>, S<6,2, B<4,0>>>                          operand_cimmsh6;
typedef simm_operand_t<6, S<12,12, B<5>>, S<6,2, B<4,0>>>                          operand_cimmi;
typedef simm_operand_t<6, S<12,12, B<5>>, S<6,2, B<4,0>>>                          operand_cnzimmi;
typedef simm_operand_t<18, S<12,12, B<17>>, S<6,2, B<16,12>>>                      operand_cimmui;
typedef uimm_operand_t<8, S<12,12, B<5>>, S<6,2, B<4,2>,B<7,6>>>                   operand_cimmlwsp;
typedef uimm_operand_t<9, S<12,12, B<5>>, S<6,2, B<4,3>,B<8,6>>>                   operand_cimmldsp;
typedef uimm_operand_t<10, S<12,12, B<5>>, S<6,2, B<4>,B<9,6>>>                    operand_cimmlqsp;
typedef simm_operand_t<10, S<12,12, B<9>>, S<6,2, B<4>,B<6>,B<8,7>,B<5>>>          operand_cimm16sp;
typedef simm_operand_t<12, S<12,2, B<11>,B<4>,B<9,8>,B<10>,B<6>,B<7>,B<3,1>,B<5>>> operand_cimmj;
typedef simm_operand_t<9, S<12,10, B<8>,B<4,3>>, S<6,2, B<7,6>,B<2,1>,B<5>>>       operand_cimmb;
typedef uimm_operand_t<8, S<12,7, B<5,2>,B<7,6>>>                                  operand_cimmswsp;
typedef uimm_operand_t<9, S<12,7, B<5,3>,B<8,6>>>                                  operand_cimmsdsp;
typedef uimm_operand_t<10, S<12,7, B<5,4>,B<9,6>>>                                 operand_cimmsqsp;
typedef uimm_operand_t<10, S<12,5, B<5,4>,B<9,6>,B<2>,B<3>>>                       operand_cimm4spn;
typedef uimm_operand_t<7, S<12,10, B<5,3>>, S<6,5, B<2>,B<6>>>                     operand_cimmw;
typedef uimm_operand_t<8, S<12,10, B<5,3>>, S<6,5, B<7,6>>>                        operand_cimmd;
typedef uimm_operand_t<9, S<12,10, B<5,4>,B<8>>, S<6,5, B<7,6>>>                   operand_cimmq;

#endif
