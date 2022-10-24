(* Wolfram Language Script to solve asset allocation with negative Exponential Utility and the Laplace distribution *)
f[x] = PDF[LaplaceDistribution[\[Alpha], \[Sigma]], x]
U[W_, \[Lambda]_] := Exp[-\[Lambda] W]
u = Simplify[
  Assuming[\[Sigma] > 0 \[And] \[Lambda] > 0 \[And] 
    h \[Element] Reals \[And] Abs[h \[Lambda] \[Sigma]] < 1, 
   Integrate[
    U[h x, \[Lambda]] f[x], {x, -\[Infinity], \[Infinity]}]]]
d = Simplify[Solve[D[u, h] == 0, h], 
  Assumptions -> \[Lambda] > 0 \[And] \[Sigma] > 0]
ph = h /. Last[d] /. {\[Lambda] -> 1, \[Sigma] -> 1}
nh = h /. First[d] /. {\[Lambda] -> 1, \[Sigma] -> 1}

(* plot the positive and negative solutions *)
Export["laplace.pdf",Plot[{ph, nh, 1, -1, \[Alpha]/2}, {\[Alpha], -5, 5}, Frame -> True, 
 PlotStyle -> {Blue, Red, LightGray, LightGray, Dotted}, 
 FrameLabel -> 
  Map[Style[#, Larger] &, {"\[Alpha]/\[Sigma]", \[Lambda]h}], 
 ImageSize -> 600, 
 PlotLegends -> 
  Placed[{"Positive Solution", "Negative Solution"}, Above]]]

