(* ::Package:: *)

BeginPackage["PeterBurbery`MixedGraphs`"];

(* Declare your packages public symbols here. *)

PeterBurbery`MixedGraphs`RandomMixedGraph;
PeterBurbery`MixedGraphs`EulerizeGraph;
PeterBurbery`MixedGraphs`UndirectedGraphToMixedGraph;
PeterBurbery`MixedGraphs`RandomWeightedMixedGraph;
Begin["`Private`"];

(* Define your public and private symbols here. *)

ClearAll[RandomMixedGraph];


RandomMixedGraph[{vertices_,edges_},threshold_]/;0<=threshold<=1:=Block[{replaceCount,randomGraph} ,randomGraph=RandomGraph[{vertices,edges}];replaceCount=Floor[threshold edges];Graph[Fold[EdgeAdd[EdgeDelete[#1,#2],First[#2]\[DirectedEdge]Last[#2]]&,randomGraph,RandomSample[EdgeList[randomGraph],replaceCount]],EdgeWeight->Thread[EdgeList[randomGraph]->(AnnotationValue[{randomGraph,#1},EdgeWeight])&/@EdgeList[randomGraph]]]];

RandomMixedGraph[{vertices_,edges_},threshold_,k_?IntegerQ]/;0<=threshold<=1:=Block[{replaceCount,randomGraphList} ,randomGraphList=RandomGraph[{vertices,edges},k];replaceCount=Floor[threshold edges];(randomGraph|->Graph[Fold[EdgeAdd[EdgeDelete[#1,#2],First[#2]\[DirectedEdge]Last[#2]]&,randomGraph,RandomSample[EdgeList[randomGraph],replaceCount]],EdgeWeight->Thread[EdgeList[randomGraph]->(AnnotationValue[{randomGraph,#1},EdgeWeight])&/@EdgeList[randomGraph]]])/@randomGraphList];

RandomMixedGraph[{vertices_,edges_},threshold_,array_List]/;0<=threshold<=1:=ArrayReshape[Table[(randomGraph|->Graph[Fold[EdgeAdd[EdgeDelete[#1,#2],First[#2]\[DirectedEdge]Last[#2]]&,randomGraph,RandomSample[EdgeList[randomGraph],\[LeftFloor]threshold EdgeCount[randomGraph]\[RightFloor]]],EdgeWeight->Thread[EdgeList[randomGraph]->(AnnotationValue[{randomGraph,#1},EdgeWeight])&/@EdgeList[randomGraph]]])[k],{k,Flatten[RandomGraph[{vertices,edges},array]]}],array]

RandomMixedGraph[graphDistribution_,threshold_]/;0<=threshold<=1:=Block[{replaceCount,randomGraph} ,randomGraph=RandomGraph[graphDistribution];replaceCount=Floor[threshold EdgeCount[randomGraph]];Graph[Fold[EdgeAdd[EdgeDelete[#1,#2],First[#2]\[DirectedEdge]Last[#2]]&,randomGraph,RandomSample[EdgeList[randomGraph],replaceCount]],EdgeWeight->Thread[EdgeList[randomGraph]->(AnnotationValue[{randomGraph,#1},EdgeWeight])&/@EdgeList[randomGraph]]]]

RandomMixedGraph[graphDistribution_,threshold_,k_?IntegerQ]/;0<=threshold<=1\[And]k>=1:=Block[{randomGraphList} ,randomGraphList=RandomGraph[graphDistribution,k];(randomGraph|->Graph[Fold[EdgeAdd[EdgeDelete[#1,#2],First[#2]\[DirectedEdge]Last[#2]]&,randomGraph,RandomSample[EdgeList[randomGraph],Floor[threshold EdgeCount[randomGraph]]]],EdgeWeight->Thread[EdgeList[randomGraph]->(AnnotationValue[{randomGraph,#1},EdgeWeight])&/@EdgeList[randomGraph]]])/@randomGraphList];

RandomMixedGraph[graphDistribution_,threshold_,array_List]/;0<=threshold<=1:=ArrayReshape[Table[(randomGraph|->Graph[Fold[EdgeAdd[EdgeDelete[#1,#2],First[#2]\[DirectedEdge]Last[#2]]&,randomGraph,RandomSample[EdgeList[randomGraph],\[LeftFloor]threshold EdgeCount[randomGraph]\[RightFloor]]],EdgeWeight->Thread[EdgeList[randomGraph]->(AnnotationValue[{randomGraph,#1},EdgeWeight])&/@EdgeList[randomGraph]]])[k],{k,Flatten[RandomGraph[graphDistribution,array]]}],array]

RandomMixedGraph::usage="RandomMixedGraph[{\!\(\*
StyleBox[\"vertices\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"edges\",\nFontSlant->\"Italic\"]\)}, \!\(\*
StyleBox[\"threshold\",\nFontSlant->\"Italic\"]\)] creates a random graph with vertices \!\(\*
StyleBox[\"vertices\",\nFontSlant->\"Italic\"]\) and edges \!\(\*
StyleBox[\"edges\",\nFontSlant->\"Italic\"]\) with a fraction \!\(\*
StyleBox[\"threshold\",\nFontSlant->\"Italic\"]\) converted to directed arcs\nRandomMixedGraph[{\!\(\*
StyleBox[\"vertices\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"edges\",\nFontSlant->\"Italic\"]\)}, \!\(\*
StyleBox[\"threshold\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"k\",\nFontSlant->\"Italic\"]\)] creates a list of \!\(\*
StyleBox[\"k\",\nFontSlant->\"Italic\"]\) random mixed graphs that have a fraction \!\(\*
StyleBox[\"threshold\",\nFontSlant->\"Italic\"]\) of directed arcs between 0 and 1 with \!\(\*
StyleBox[\"vertices\",\nFontSlant->\"Italic\"]\) vertices and \!\(\*
StyleBox[\"edges\",\nFontSlant->\"Italic\"]\) edges\nRandomMixedGraph[{\!\(\*
StyleBox[\"vertices\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"edges\",\nFontSlant->\"Italic\"]\)}, \!\(\*
StyleBox[\"threshold\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"array\",\nFontSlant->\"Italic\"]\)] creates an array with dimensions \!\(\*
StyleBox[\"array\",\nFontSlant->\"Italic\"]\) of random mixed graphs with \!\(\*
StyleBox[\"vertices\",\nFontSlant->\"Italic\"]\) vertices and \!\(\*
StyleBox[\"edges\",\nFontSlant->\"Italic\"]\) edges and \!\(\*
StyleBox[\"threshold\",\nFontSlant->\"Italic\"]\) directed edges\nRandomMixedGraph[\!\(\*
StyleBox[\"distribution\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"threshold\",\nFontSlant->\"Italic\"]\)] creates an undirected graph from \!\(\*
StyleBox[\"distribution\",\nFontSlant->\"Italic\"]\) and converts \!\(\*
StyleBox[\"threshold\",\nFontSlant->\"Italic\"]\) of the undirected edges into directed edges to make a mixed graph\nRandomMixedGraph[\!\(\*
StyleBox[\"distribution\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"threshold\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"k\",\nFontSlant->\"Italic\"]\)] creates a list of \!\(\*
StyleBox[\"k\",\nFontSlant->\"Italic\"]\) undirected graphs from \!\(\*
StyleBox[\"distribution\",\nFontSlant->\"Italic\"]\) and converts \!\(\*
StyleBox[\"threshold\",\nFontSlant->\"Italic\"]\) of the undirected edges into directed arcs to make mixed graphs\nRandomMixedGraph[\!\(\*
StyleBox[\"distribution\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"threshold\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"array\",\nFontSlant->\"Italic\"]\)] creates a list of undirected graphs with dimensions \!\(\*
StyleBox[\"array\",\nFontSlant->\"Italic\"]\) from \!\(\*
StyleBox[\"distribution\",\nFontSlant->\"Italic\"]\) and converts \!\(\*
StyleBox[\"threshold\",\nFontSlant->\"Italic\"]\) of the undirected edges into directed arcs to make mixed graphs";
ClearAll[UndirectedGraphToMixedGraph]
UndirectedGraphToMixedGraph[graph_?GraphQ,threshold_]:=Block[{replaceCount} ,replaceCount=Floor[threshold EdgeCount[graph]];Graph[Fold[EdgeAdd[EdgeDelete[#1,#2],First[#2]\[DirectedEdge]Last[#2]]&,graph,RandomSample[EdgeList[graph],replaceCount]],EdgeWeight->Thread[EdgeList[graph]->(AnnotationValue[{graph,#1},EdgeWeight])&/@EdgeList[graph]]]]/;0<=threshold<=1
ClearAll[RandomWeightedMixedGraph]
RandomWeightedMixedGraph[{vertices_,edges_},threshold_,randomfunction_]/;0<=threshold<=1:=Block[{replaceCount,randomGraph} ,randomGraph=RandomGraph[{vertices,edges}];replaceCount=Floor[threshold edges];Graph[Fold[EdgeAdd[EdgeDelete[#1,#2],First[#2]\[DirectedEdge]Last[#2]]&,randomGraph,RandomSample[EdgeList[randomGraph],replaceCount]],EdgeWeight->Thread[EdgeList[randomGraph]->(AnnotationValue[{randomGraph,#1},EdgeWeight])&/@EdgeList[randomGraph]]];Graph[randomGraph,Table[randomfunction[k],{k,EdgeCount[randomGraph]}]]];
End[]; (* End `Private` *)

EndPackage[];



