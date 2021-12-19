(* ::Package:: *)

expriDir=FileNameJoin[{$srcRoot,"expriment"}];


expriDataset=Get[FileNameJoin[{expriDir,"nucleon-data.auth-year.wl"}]];


ListPlot[
Values[assoc`expr[[inde]]],
PlotRange->{Full,Full},
AxesOrigin->{0,0},
PlotRangePadding->{{0,0},{Scaled[0.09],Scaled[0.12]}},
PlotRangeClipping->True,
ClippingStyle->Automatic,
Sequence@@marker`expr`sequence[expr`errobar`style,inde]
]


ListPlot[Query[
Key@ff["n"],ffsGEGM,1
]@expriDataset]
