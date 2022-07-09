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

RandomMixedGraph::usage="RandomMixedGraph[{vertices, edges}, threshold] creates a random graph with vertices and edges with a fraction threshold converted to directed arcs\nRandomMixedGraph[{vertices, edges}, threshold, k] creates a list of k random mixed graphs that have a fraction threshold of directed arcs between 0 and 1 with a certain number of vertices and edges\nRandomMixedGraph[{vertices, edges}, threshold, array] creates an array with dimensions array of random mixed graphs with vertices vertices and edges edges and threshold directed edges\nRandomMixedGraph[distribution, threshold] creates an undirected graph from distribution and converts threshold of the undirected edges into directed edges to make a mixed graph\nRandomMixedGraph[distribution, threshold, k] creates a list of k undirected graphs from distribution and converts threshold of the undirected edges into directed arcs to make mixed graphs\nRandomMixedGraph[distribution, threshhold, array] creates a list of undirected graphs with dimensions array from distribution and converts threshhold of the undirected edges into directed arcs to make mixed graphs";ClearAll[EulerizeGraph]
EulerizeGraph[graph_?ConnectedGraphQ]:=NestWhile[(x|->EdgeAdd[x,UndirectedEdge[First[#],Last[#]]&@First@TakeDrop[Select[VertexList[x],OddQ[VertexDegree[x,#]]&],2]]),graph,!EulerianGraphQ[#]&]
ClearAll[UndirectedGraphToMixedGraph]
UndirectedGraphToMixedGraph[graph_?GraphQ,threshold_]:=Block[{replaceCount} ,replaceCount=Floor[threshold EdgeCount[graph]];Graph[Fold[EdgeAdd[EdgeDelete[#1,#2],First[#2]\[DirectedEdge]Last[#2]]&,graph,RandomSample[EdgeList[graph],replaceCount]],EdgeWeight->Thread[EdgeList[graph]->(AnnotationValue[{graph,#1},EdgeWeight])&/@EdgeList[graph]]]]/;0<=threshold<=1
ClearAll[RandomWeightedMixedGraph]
RandomWeightedMixedGraph[{vertices_,edges_},threshold_,randomfunction_]/;0<=threshold<=1:=Block[{replaceCount,randomGraph} ,randomGraph=RandomGraph[{vertices,edges}];replaceCount=Floor[threshold edges];Graph[Fold[EdgeAdd[EdgeDelete[#1,#2],First[#2]\[DirectedEdge]Last[#2]]&,randomGraph,RandomSample[EdgeList[randomGraph],replaceCount]],EdgeWeight->Thread[EdgeList[randomGraph]->(AnnotationValue[{randomGraph,#1},EdgeWeight])&/@EdgeList[randomGraph]]];Graph[randomGraph,Table[randomfunction[k],{k,EdgeCount[randomGraph]}]]];
End[]; (* End `Private` *)

EndPackage[];



