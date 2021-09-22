type plf =
        |Var of int
        |Not of plf
        |And of plf * plf
        |Or of plf * plf
;;