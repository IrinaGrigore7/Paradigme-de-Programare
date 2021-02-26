%Tbl = [[X1, Y1, Z1], [X2, Y2, Z2], [X3, Y3, Z3]]
diags(Tbl, R) :- Tbl = [[X1, Y1, Z1], [X2, Y2, Z2], [X3, Y3, Z3]], R = [[X1, Y2, Z3], [X3, Y2, Z1]].