(* ::Package:: *)

BeginPackage["PeterBurbery`MixedGraphs`"];

(* Declare your packages public symbols here. *)

PeterBurbery`MixedGraphs`RandomMixedGraph;
PeterBurbery`MixedGraphs`EulerizeGraph;
PeterBurbery`MixedGraphs`UndirectedGraphToMixedGraph;
PeterBurbery`MixedGraphs`RandomWeightedMixedGraph;
PeterBurbery`MixedGraphs`MixedGraphDirectedArcs;
PeterBurbery`MixedGraphs`MixedGraphUndirectedEdges;
PeterBurbery`MixedGraphs`GraphInformation;
PeterBurbery`MixedGraphs`TakeLargestGraphComponentBy;
PeterBurbery`MixedGraphs`GraphicalDegreeSequenceQ;
PeterBurbery`MixedGraphs`GraphConvexHull;
Begin["`Private`"];

(* Define your public and private symbols here. *)

ClearAll[RandomMixedGraph];
RandomMixedGraph[{vertices_,edges_},threshold_,options:OptionsPattern[]]/;0<=threshold<=1:=Block[{replaceCount,randomGraph} ,randomGraph=RandomGraph[{vertices,edges},options];replaceCount=Floor[threshold edges];randomGraph=Graph[Fold[EdgeAdd[EdgeDelete[#1,#2],First[#2]\[DirectedEdge]Last[#2]]&,randomGraph,RandomSample[EdgeList[randomGraph],replaceCount]]]];

RandomMixedGraph[{vertices_,edges_},threshold_,k_?IntegerQ,options:OptionsPattern[]]/;0<=threshold<=1:=Table[RandomMixedGraph[{vertices,edges},threshold,options],k]

RandomMixedGraph[{vertices_,edges_},threshold_,array_List,options:OptionsPattern[]]/;0<=threshold<=1:=Array[RandomMixedGraph[{vertices,edges},threshold,options]&,array]

RandomMixedGraph[graphDistribution_,threshold_,options:OptionsPattern[]]/;0<=threshold<=1:=Block[{replaceCount,randomGraph} ,randomGraph=RandomGraph[graphDistribution,options];replaceCount=Floor[threshold EdgeCount[randomGraph]];randomGraph=Graph[Fold[EdgeAdd[EdgeDelete[#1,#2],First[#2]\[DirectedEdge]Last[#2]]&,randomGraph,RandomSample[EdgeList[randomGraph],replaceCount]]]];

RandomMixedGraph[graphDistribution_,threshold_,k_?IntegerQ,options:OptionsPattern[]]/;0<=threshold<=1\[And]k>=1:=Table[RandomMixedGraph[graphDistribution,threshold,options],k]

RandomMixedGraph[graphDistribution_,threshold_,array_List,options:OptionsPattern[]]/;0<=threshold<=1:=Array[RandomMixedGraph[graphDistribution,threshold,options]&,array]


RandomMixedGraph::usage ="RandomMixedGraph[{vertices, edges}, threshold] creates a random mixed graph with vertices vertices and edges edges where directed edges make up threshold of the entirenumber of edges\nRandomMixedGraph[{vertices, edges}, threshold, k] creates k random mixed graphs with vertices vertices and edges edges where directed edges make up threshold of the entire number of edges\nRandomMixedGraph[{vertices, edges}, threshold, randomFunction, array] creates an array of dimensions array of random mixed graphs with vertices vertices and edges edges where directed edges make up threshold of the entire number of edges \nRandomMixedGraph[distribution, threshold, randomFunction] creates a random mixed graph with graph distribution distribution where directed edges make up threshold of the entire number of edges \nRandomMixedGraph[distribution, threshold, randomFunction, k] creates k random mixed graphs with graph distribution distribution where directed edges make up threshold of the entire number of edges with edge weights assigned by randomFunction\nRandomMixedGraph[distribution, threshold, array] creates an array of dimensions array of random mixed graphs with graph distribution distribution where directed edges make up threshold of the entire number of edges";

ClearAll[UndirectedGraphToMixedGraph]
UndirectedGraphToMixedGraph[graph_?GraphQ,threshold_]:=Block[{replaceCount} ,replaceCount=Floor[threshold EdgeCount[graph]];Graph[Fold[EdgeAdd[EdgeDelete[#1,#2],First[#2]\[DirectedEdge]Last[#2]]&,graph,RandomSample[EdgeList[graph],replaceCount]],EdgeWeight->Thread[EdgeList[graph]->(AnnotationValue[{graph,#1},EdgeWeight])&/@EdgeList[graph]]]]/;0<=threshold<=1
ClearAll[RandomWeightedMixedGraph]
RandomWeightedMixedGraph[{vertices_,edges_},threshold_,randomFunction_,options:OptionsPattern[]]/;0<=threshold<=1:=Block[{replaceCount,randomGraph} ,randomGraph=RandomGraph[{vertices,edges},options];replaceCount=Floor[threshold edges];randomGraph=Graph[Fold[EdgeAdd[EdgeDelete[#1,#2],First[#2]\[DirectedEdge]Last[#2]]&,randomGraph,RandomSample[EdgeList[randomGraph],replaceCount]]];Graph[randomGraph,EdgeWeight->(Map[#->randomFunction[]&,EdgeList[randomGraph]])]];

RandomWeightedMixedGraph[{vertices_,edges_},threshold_,randomFunction_,k_?IntegerQ,options:OptionsPattern[]]/;0<=threshold<=1:=Table[RandomWeightedMixedGraph[{vertices,edges},threshold,randomFunction],k]

RandomWeightedMixedGraph[{vertices_,edges_},threshold_,randomFunction_,array_List,options:OptionsPattern[]]/;0<=threshold<=1:=Array[RandomWeightedMixedGraph[{vertices,edges},threshold,randomFunction]&,array]

RandomWeightedMixedGraph[graphDistribution_,threshold_,randomFunction_,options:OptionsPattern[]]/;0<=threshold<=1:=Block[{replaceCount,randomGraph} ,randomGraph=RandomGraph[graphDistribution,options];replaceCount=Floor[threshold EdgeCount[randomGraph]];randomGraph=Graph[Fold[EdgeAdd[EdgeDelete[#1,#2],First[#2]\[DirectedEdge]Last[#2]]&,randomGraph,RandomSample[EdgeList[randomGraph],replaceCount]]];Graph[randomGraph,EdgeWeight->(Map[#->randomFunction[]&,EdgeList[randomGraph]])]];

RandomWeightedMixedGraph[graphDistribution_,threshold_,randomFunction_,k_?IntegerQ,options:OptionsPattern[]]/;0<=threshold<=1\[And]k>=1:=Table[RandomWeightedMixedGraph[graphDistribution,threshold,randomFunction],k]

RandomWeightedMixedGraph[graphDistribution_,threshold_,randomFunction_,array_List,options:OptionsPattern[]]/;0<=threshold<=1:=Array[RandomWeightedMixedGraph[graphDistribution,threshold,randomFunction]&,array]
RandomWeightedMixedGraph::usage="RandomWeightedMixedGraph[{vertices, edges}, threshold, randomFunction] creates a random mixed graph with vertices vertices and edges edges where directed edges make up threshold of the entire number of edges with edge weights assigned by randomFunction\nRandomWeightedMixedGraph[{vertices, edges}, threshold, randomFunction, k] creates k random mixed graphs with vertices vertices and edges edges where directed edges make up threshold of the entire number of edges with edge weights assigned by randomFunction\nRandomWeightedMixedGraph[{vertices, edges}, threshold, randomFunction, array] creates an array of dimensions array of random mixed graphs with vertices vertices and edges edges where directed edges make up threshold of the entire number of edges with edge weights assigned by randomFunction\nRandomWeightedMixedGraph[distribution, threshold, randomFunction] creates a random mixed graph with graph distribution distribution where directed edges make up threshold of the entire number of edges with edge weights assigned by randomFunction\nRandomWeightedMixedGraph[distribution, threshold, randomFunction, k] creates k random mixed graphs with graph distribution distribution where directed edges make up threshold of the entire number of edges with edge weights assigned by randomFunction\nRandomWeightedMixedGraph[distribution, threshold, randomFunction, array] creates an array of dimensions array of random mixed graphs with graph distribution distribution where directed edges make up threshold of the entire number of edges with edge weights assigned by randomFunction";
ClearAll[MixedGraphDirectedArcs]
MixedGraphDirectedArcs[graph_?GraphQ]:=Cases[EdgeList[graph],_DirectedEdge]
ClearAll[MixedGraphUndirectedEdges]
MixedGraphUndirectedEdges[graph_?GraphQ]:=Cases[EdgeList[graph],_UndirectedEdge]

ClearAll[EulerizeGraph]
EulerizeGraph[graph_?ConnectedGraphQ]:=NestWhile[(x|->EdgeAdd[x,UndirectedEdge[First[#],Last[#]]&@First@TakeDrop[Select[VertexList[x],OddQ[VertexDegree[x,#]]&],2]]),graph,!EulerianGraphQ[#]&]

ClearAll[GraphInformation]
GraphInformation[graph_?GraphQ]:=<|"Acyclic"->AcyclicGraphQ[graph],"Bipartite"->BipartiteGraphQ[graph],"Complete"->CompleteGraphQ[graph],"Connected"->ConnectedGraphQ[graph],"EdgeTransitive"->EdgeTransitiveGraphQ[graph],"WeightedEdge"->EdgeWeightedGraphQ[graph],"Empty"->EmptyGraphQ[graph],"Eulerian"->EulerianGraphQ[graph],"Hamiltonian"->HamiltonianGraphQ[graph],"LoopFree"->LoopFreeGraphQ[graph],"Mixed"->MixedGraphQ[graph],"Path"->PathGraphQ[graph],"Planar"->PlanarGraphQ[graph],"Simple"->SimpleGraphQ[graph],"Tree"->TreeGraphQ[graph],"Undirected"->UndirectedGraphQ[graph],"VertexTransitive"->VertexTransitiveGraphQ[graph],"WeightedVertex"->VertexWeightedGraphQ[graph],"WeaklyConnected"->WeaklyConnectedGraphQ[graph],"Weighted"->WeightedGraphQ[graph]|>

ClearAll[TakeLargestGraphComponentBy]
TakeLargestGraphComponentBy[graph_?GraphQ,function_:EdgeCount,length_:1]:=TakeLargestBy[ConnectedGraphComponents[graph],function,length]

ClearAll[GraphicalDegreeSequenceQ]

GraphicalDegreeSequenceQ[sequence:{___Integer}]:=EvenQ[Total[sequence]]&&Block[{orderedSequence},orderedSequence=ReverseSort[sequence];ContainsOnly[(k|->( Sum[orderedSequence[[i]],{i,1,k}]<=k(k-1)+Sum[Min[{orderedSequence[[i]],k}],{i,k+1,Length[sequence]}]))/@Range[Length[sequence]],{True}]]
ClearAll[GraphConvexHull]
GraphConvexHull[graph_?GraphQ,vertexSet_]:=FixedPoint[Function[in,Union@Flatten@Function[{g,v},FindPath[g,First[#],Last[#],GraphDistance[g,First[#],Last[#]],All]&/@Subsets[v,{2}]][example,in]][#]&,{1,2,9}]/;SubsetQ[VertexList[graph],vertexSet]
End[]; (* End `Private` *)

EndPackage[];
