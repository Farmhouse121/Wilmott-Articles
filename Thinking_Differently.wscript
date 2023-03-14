(* Script to compute figures for Wilmott article: Thinking Differently About Asset Allocation, runs in WolframScript https://www.wolfram.com/wolframscript/ *)

(* empty the workspace *)
Clear[\[Alpha],V,\[Sigma],Z,\[CapitalDelta],\[CapitalOmega]]

(* emit a vector of alphas with subscripts of given dimension *)
\[Alpha][n_Integer]:=Map[Subscript[\[Alpha],#]&,Range[n]]

(* emit a subscripted sigma *)
\[Sigma][i_Integer]:=Subscript[\[Sigma],i] 

(* emit a subscripted covariance *)
\[Sigma][i_Integer,j_Integer]:=\[Sigma][i]\[Sigma][j]Which[i>j,Subscript[\[Rho],j,i],i<j,Subscript[\[Rho],i,j],True,1] 

(* build a symbolic covariance matrix of given dimension *)
V[n_Integer]:=Module[{i,j},Table[Table[\[Sigma][i,j],{i,n}],{j,n}]] 

(* Mahanalobis distance constructed from alpha vector and covariance matrix *)
\[CapitalDelta][\[Alpha]_,V_]:=Sqrt[\[Alpha].Inverse[V].\[Alpha]] 

(* candidate portfolio weighting function as a function of Mahanalobis distance *)
\[CapitalOmega][Z_]:=(Sqrt[1+Z^2]-1)/Z^2 

(* candidate holding function of given dimension as product of Markowitz/Kelly vector and p.w.f. *)
h[n_Integer]:=Inverse[V[n]].\[Alpha][n]\[CapitalOmega][\[CapitalDelta][\[Alpha][n],V[n]]] 

(* emit a subscripted Z *)
Z[i_Integer]:=Subscript[Z,i]

(* symbolic representation of p.w.f. for two correlated assets *)
\[CapitalOmega]zz=Simplify[\[CapitalOmega][\[CapitalDelta][\[Alpha][2],V[2]]]/.Table[Subscript[\[Alpha],i]->Z[i]\[Sigma][i],{i,2}]/.Subscript[\[Rho],1,2]->\[Rho]]

(* output the p.w.f. for two independent assets *)
Plot3D[{\[CapitalOmega]zz/.\[Rho]->0,0},{Z[1],-5,5},{Z[2],-5,5},MaxRecursion->10,ImageSize->800,Mesh->{10,10,{0},{0}},MeshFunctions->{#1&,#2&,(#1-#2)&,(#1+#2)&},MeshStyle->{Automatic,Automatic,{Green,Thick},{Red,Thick}},AxesLabel->Map[Style[#,Larger]&,{Z[1],Z[2],\[CapitalOmega][Z[1],Z[2]]}]] 
Export["iso.png",%,ImageResolution->300] 

(* output the p.w.f. for two positively correlated assets *)
Plot3D[{\[CapitalOmega]zz/.\[Rho]->0.45,0},{Z[1],-5,5},{Z[2],-5,5},MaxRecursion->10,PlotPoints->100,ImageSize->800,Mesh->{10,10,{0},{0}},MeshFunctions->{#1&,#2&,(#1-#2)&,(#1+#2)&},MeshStyle->{Automatic,Automatic,{Green,Thick},{Red,Thick}},AxesLabel->Map[Style[#,Larger]&,{Z[1],Z[2],\[CapitalOmega][Z[1],Z[2]]}],PlotRange->All,PreserveImageOptions->False]
Export["pos.png",%,ImageResolution->300]

(* output the p.w.f. for two negatively correlated assets *)
Plot3D[{\[CapitalOmega]zz/.\[Rho]->-0.95,0},{Z[1],-5,5},{Z[2],-5,5},MaxRecursion->15,PlotPoints->100,ImageSize->800,Mesh->{10,10,{0},{0}},MeshFunctions->{#1&,#2&,(#1-#2)&,(#1+#2)&},MeshStyle->{Automatic,Automatic,{Green,Thick},{Red,Thick}},AxesLabel->Map[Style[#,Larger]&,{Z[1],Z[2],\[CapitalOmega][Z[1],Z[2]]}],PlotRange->Full,PreserveImageOptions->False]
Export["neg.png",%,ImageResolution->300]

(* compute Taylor series and limits *)
Series[\[CapitalOmega][Z],{Z,0,3}]
Limit[Z \[CapitalOmega][Z],Z->\[Infinity]]

(* output the p.w.f. as a function of Manhanalobis distance with limits *)
Plot[{\[CapitalOmega][Z],1/2,{1/Abs[Z]}},{Z,-10,10},PlotRange->{0,0.6},Frame->True,ImageSize->800,PlotStyle->{{Blue},{Red,Dotted},{Gray,Dotted}},FrameLabel->Map[Style[#,Larger]&,{"Mahanalobis Distance","Weighting Factor"}]]
Export["mahanalobis.png",%,ImageResolution->300]

(* compute the limiting ratio of the p.w.f. for totally symmetric and anti-symmwetric Z-scores *)
Limit[(\[CapitalOmega]zz/.Z[2]->Z[1])/(\[CapitalOmega]zz/.Z[2]->-Z[1]),Z[1]->\[Infinity]]
Simplify[%,Assumptions->-1<=\[Rho]<1]
