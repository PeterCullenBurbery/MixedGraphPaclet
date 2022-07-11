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
RandomMixedGraph[{vertices_,edges_},threshold_]/;0<=threshold<=1:=Block[{replaceCount,randomGraph} ,randomGraph=RandomGraph[{vertices,edges}];replaceCount=Floor[threshold edges];randomGraph=Graph[Fold[EdgeAdd[EdgeDelete[#1,#2],First[#2]\[DirectedEdge]Last[#2]]&,randomGraph,RandomSample[EdgeList[randomGraph],replaceCount]]]];

RandomMixedGraph[{vertices_,edges_},threshold_,k_?IntegerQ]/;0<=threshold<=1:=Table[RandomMixedGraph[{vertices,edges},threshold,randomFunction],k]

RandomMixedGraph[{vertices_,edges_},threshold_,array_List]/;0<=threshold<=1:=Array[RandomMixedGraph[{vertices,edges},threshold,randomFunction]&,array]

RandomMixedGraph[graphDistribution_,threshold_]/;0<=threshold<=1:=Block[{replaceCount,randomGraph} ,randomGraph=RandomGraph[graphDistribution];replaceCount=Floor[threshold EdgeCount[randomGraph]];randomGraph=Graph[Fold[EdgeAdd[EdgeDelete[#1,#2],First[#2]\[DirectedEdge]Last[#2]]&,randomGraph,RandomSample[EdgeList[randomGraph],replaceCount]]]];

RandomMixedGraph[graphDistribution_,threshold_,k_?IntegerQ]/;0<=threshold<=1\[And]k>=1:=Table[RandomMixedGraph[graphDistribution,threshold,randomFunction],k]

RandomMixedGraph[graphDistribution_,threshold_,array_List]/;0<=threshold<=1:=Array[RandomMixedGraph[graphDistribution,threshold,randomFunction]&,array]


RandomMixedGraph::usage ="RandomMixedGraph[{vertices, edges}, threshold] creates a random mixed graph with vertices vertices and edges edges where directed edges make up threshold of the entirenumber of edges\nRandomMixedGraph[{vertices, edges}, threshold, k] creates k random mixed graphs with vertices vertices and edges edges where directed edges make up threshold of the entire number of edges\nRandomMixedGraph[{vertices, edges}, threshold, randomFunction, array] creates an array of dimensions array of random mixed graphs with vertices vertices and edges edges where directed edges make up threshold of the entire number of edges \nRandomMixedGraph[distribution, threshold, randomFunction] creates a random mixed graph with graph distribution distribution where directed edges make up threshold of the entire number of edges \nRandomMixedGraph[distribution, threshold, randomFunction, k] creates k random mixed graphs with graph distribution distribution where directed edges make up threshold of the entire number of edges with edge weights assigned by randomFunction\nRandomMixedGraph[distribution, threshold, array] creates an array of dimensions array of random mixed graphs with graph distribution distribution where directed edges make up threshold of the entire number of edges";

ClearAll[UndirectedGraphToMixedGraph]
UndirectedGraphToMixedGraph[graph_?GraphQ,threshold_]:=Block[{replaceCount} ,replaceCount=Floor[threshold EdgeCount[graph]];Graph[Fold[EdgeAdd[EdgeDelete[#1,#2],First[#2]\[DirectedEdge]Last[#2]]&,graph,RandomSample[EdgeList[graph],replaceCount]],EdgeWeight->Thread[EdgeList[graph]->(AnnotationValue[{graph,#1},EdgeWeight])&/@EdgeList[graph]]]]/;0<=threshold<=1
ClearAll[RandomWeightedMixedGraph]
RandomWeightedMixedGraph[{vertices_,edges_},threshold_,randomFunction_]/;0<=threshold<=1:=Block[{replaceCount,randomGraph} ,randomGraph=RandomGraph[{vertices,edges}];replaceCount=Floor[threshold edges];randomGraph=Graph[Fold[EdgeAdd[EdgeDelete[#1,#2],First[#2]\[DirectedEdge]Last[#2]]&,randomGraph,RandomSample[EdgeList[randomGraph],replaceCount]]];Graph[randomGraph,EdgeWeight->(Map[#->randomFunction[]&,EdgeList[randomGraph]])]];

RandomWeightedMixedGraph[{vertices_,edges_},threshold_,randomFunction_,k_?IntegerQ]/;0<=threshold<=1:=Table[RandomWeightedMixedGraph[{vertices,edges},threshold,randomFunction],k]

RandomWeightedMixedGraph[{vertices_,edges_},threshold_,randomFunction_,array_List]/;0<=threshold<=1:=Array[RandomWeightedMixedGraph[{vertices,edges},threshold,randomFunction]&,array]

RandomWeightedMixedGraph[graphDistribution_,threshold_,randomFunction_]/;0<=threshold<=1:=Block[{replaceCount,randomGraph} ,randomGraph=RandomGraph[graphDistribution];replaceCount=Floor[threshold EdgeCount[randomGraph]];randomGraph=Graph[Fold[EdgeAdd[EdgeDelete[#1,#2],First[#2]\[DirectedEdge]Last[#2]]&,randomGraph,RandomSample[EdgeList[randomGraph],replaceCount]]];Graph[randomGraph,EdgeWeight->(Map[#->randomFunction[]&,EdgeList[randomGraph]])]];

RandomWeightedMixedGraph[graphDistribution_,threshold_,randomFunction_,k_?IntegerQ]/;0<=threshold<=1\[And]k>=1:=Table[RandomWeightedMixedGraph[graphDistribution,threshold,randomFunction],k]

RandomWeightedMixedGraph[graphDistribution_,threshold_,randomFunction_,array_List]/;0<=threshold<=1:=Array[RandomWeightedMixedGraph[graphDistribution,threshold,randomFunction]&,array]
RandomWeightedMixedGraph::usage="RandomWeightedMixedGraph[{vertices, edges}, threshold, randomFunction] creates a random mixed graph with vertices vertices and edges edges where directed edges make up threshold of the entire number of edges with edge weights assigned by randomFunction\nRandomWeightedMixedGraph[{vertices, edges}, threshold, randomFunction, k] creates k random mixed graphs with vertices vertices and edges edges where directed edges make up threshold of the entire number of edges with edge weights assigned by randomFunction\nRandomWeightedMixedGraph[{vertices, edges}, threshold, randomFunction, array] creates an array of dimensions array of random mixed graphs with vertices vertices and edges edges where directed edges make up threshold of the entire number of edges with edge weights assigned by randomFunction\nRandomWeightedMixedGraph[distribution, threshold, randomFunction] creates a random mixed graph with graph distribution distribution where directed edges make up threshold of the entire number of edges with edge weights assigned by randomFunction\nRandomWeightedMixedGraph[distribution, threshold, randomFunction, k] creates k random mixed graphs with graph distribution distribution where directed edges make up threshold of the entire number of edges with edge weights assigned by randomFunction\nRandomWeightedMixedGraph[distribution, threshold, randomFunction, array] creates an array of dimensions array of random mixed graphs with graph distribution distribution where directed edges make up threshold of the entire number of edges with edge weights assigned by randomFunction";

End[]; (* End `Private` *)

EndPackage[];
