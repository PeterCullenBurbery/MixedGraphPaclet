(* ::Package:: *)

BeginPackage["PeterBurbery`MixedGraphPaclet`"];

(* Declare your packages public symbols here. *)

RandomMixedGraph;

Begin["`Private`"];

(* Define your public and private symbols here. *)

ClearAll[RandomMixedGraph];

RandomMixedGraph[{vertices_,edges_},threshold_]/;0<=threshold<=1:=Block[{replaceCount,randomGraph} ,randomGraph=RandomGraph[{vertices,edges}];replaceCount=Floor[threshold edges];Graph[Fold[EdgeAdd[EdgeDelete[#1,#2],First[#2]\[DirectedEdge]Last[#2]]&,randomGraph,RandomSample[EdgeList[randomGraph],replaceCount]],EdgeWeight->Thread[EdgeList[randomGraph]->(AnnotationValue[{randomGraph,#1},EdgeWeight])&/@EdgeList[randomGraph]]]];

RandomMixedGraph[{vertices_,edges_},threshold_,k_]/;0<=threshold<=1:=Block[{replaceCount,randomGraphList} ,randomGraphList=RandomGraph[{vertices,edges},k];replaceCount=Floor[threshold edges];(randomGraph|->Graph[Fold[EdgeAdd[EdgeDelete[#1,#2],First[#2]\[DirectedEdge]Last[#2]]&,randomGraph,RandomSample[EdgeList[randomGraph],replaceCount]],EdgeWeight->Thread[EdgeList[randomGraph]->(AnnotationValue[{randomGraph,#1},EdgeWeight])&/@EdgeList[randomGraph]]])/@randomGraphList];

RandomMixedGraph[graphDistribution_,threshold_]/;0<=threshold<=1:=Block[{replaceCount,randomGraph} ,randomGraph=RandomGraph[graphDistribution];replaceCount=Floor[threshold EdgeCount[randomGraph]];Graph[Fold[EdgeAdd[EdgeDelete[#1,#2],First[#2]\[DirectedEdge]Last[#2]]&,randomGraph,RandomSample[EdgeList[randomGraph],replaceCount]],EdgeWeight->Thread[EdgeList[randomGraph]->(AnnotationValue[{randomGraph,#1},EdgeWeight])&/@EdgeList[randomGraph]]]]

RandomMixedGraph[graphDistribution_,threshold_,k_]/;0<=threshold<=1:=Block[{randomGraphList} ,randomGraphList=RandomGraph[graphDistribution,k];(randomGraph|->Graph[Fold[EdgeAdd[EdgeDelete[#1,#2],First[#2]\[DirectedEdge]Last[#2]]&,randomGraph,RandomSample[EdgeList[randomGraph],Floor[threshold EdgeCount[randomGraph]]]],EdgeWeight->Thread[EdgeList[randomGraph]->(AnnotationValue[{randomGraph,#1},EdgeWeight])&/@EdgeList[randomGraph]]])/@randomGraphList];

RandomMixedGraph::usage="RandomMixedGraph[{vertices,edges},threshhold] creates a random graph with vertices and edges with a fraction threshhold converted to directed arcs\nRandomMixedGraph[{vertices,edges},threshhold,k] creates an array k of random mixed graphs that have a fraction threshhold of directed arcs between 0 and 1 with a certain number of vertices and edges\nRandomMixedGraph[distribution, threshhold] creates an undirected graph from distribution and converts threshhold of the undirected edges into directed edges to make a mixed graph\nRandomMixedGraph[distribution, threshholdk] creates an array k of undirected graphs from distribution and converts threshhold of the undirected edges into directed arcs to make an array k of mixed graphs";

End[]; (* End `Private` *)

EndPackage[];
