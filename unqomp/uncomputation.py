from unqomp.dependencygraph import DependencyGraph, Node, Edge, InitGate
from unqomp.converter import ConverterDependencyGraph
from qiskit.circuit import QuantumRegister
from qiskit.converters import circuit_to_dag, dag_to_circuit
from unqomp.ancillaallocation import AncillaRegister

def uncomputeAllAncillas(circuit, extra_gates = [], linking = True, save_time = True):
    # Uncomputes and links all ancillaRegisters
    # extra_gates: couples (gate, is_qfree : Bool)
    # returns the new circuit with uncomputation if uncomputation succeeded, none otherwise
    # We have to make sure non ancilla registers are still added in the same order, 
    # so we keep them as a list before breaking our circuit down, then give it to the conversion
    registers_in_order = circuit.qubits
    dag = circuit_to_dag(circuit)
    conv = ConverterDependencyGraph(extra_gates)
    dep_g = conv.dagToDepGraph(dag)
    uncomp = Uncomputation()
    # get the nodes to uncompute, in rev topo order:
    nodes_to_uncomp = [n for n in dep_g.nodesInTopologicalOrder() if isinstance(n.register, AncillaRegister) and n.value_id > 0]
    nodes_to_uncomp.reverse()
    if linking:
        if uncomp._uncompListNodes(dep_g, nodes_to_uncomp, save_time):
            return dag_to_circuit(conv.depGraphToDagWithLinking(dep_g, registers_in_order))
    else:
        if uncomp._uncompListNodes(dep_g, nodes_to_uncomp, save_time):
            return dag_to_circuit(conv.depGraphToDag(dep_g, registers_in_order))
    return None

class Uncomputation:
    def __init__(self):
        pass

    def _uncompListNodes(self, dep_graph, nodes, latest_uncomputed = None):
        # returns False if the uncomputation failed, dep_g may have been modified
        nodes.reverse() #so that we work on the end of the list and avoid O(n) cost everywhere
        while nodes:
            cur_node = nodes.pop()
            uncomp_succeeded = self._uncomputeNode(dep_graph, cur_node)
            if not uncomp_succeeded:
                return False
        return True


    def _uncomputeNode(self, dep_graph, node):
        #returns False if the uncomputation failed, otherwise returns True
        #is this even a proper node to uncompute?
        if(node.value_id <= 0):
            print("node.val id <= 0")
            return False
        if dep_graph.nodes[node.variable_name] is None or len(dep_graph.nodes[node.variable_name]) <= node.value_id:
            return False
        #and qfree?
        if not node.gate.name in ConverterDependencyGraph.known_qfree_gates and not node.gate.name in dep_graph.extra_qfree_gates:
            print("Unqomp failed as " + node.gate.name + " is not in the list of qfree gates " + str(dep_graph.extra_qfree_gates))
            return False
        #we check that the last node on the wire has the value we want to uncompute
        latest_node_on_wire = dep_graph.nodes[node.variable_name][node.value_id][-1]
        if latest_node_on_wire.consume_edge_out is not None:
            print("Tried to uncompute node with an outgoing consume edge")
            return False
        #we now get the copy id for the new node to create
        copy_id = 1 # 0 is not uncomp, 1 is uncomp node, not many copies here
        uncomputation_node = Node(node.register, node.index, node.gate.inverse(), node.value_id - 1, copy_id)
        dep_graph.addNode(uncomputation_node)
        new_edges = dep_graph.connectConsumedNodes(latest_node_on_wire, uncomputation_node, True)

        #now we connect this new node to its controls + add avail edges
        for ctrl_edge in node.ctrl_edges_in:
            ctrl_node = ctrl_edge.node_from
            ctrl_node_latest = dep_graph.latestCopy(ctrl_node)
            dep_graph.connectDependencyNodes(ctrl_node_latest, uncomputation_node)

        # the only new edges go in and out of uncomputation_node, so any new cycle would go through it, checking this is enough to ensure the whole graph is acyclic
        return not dep_graph.hasCycleWith(uncomputation_node)
