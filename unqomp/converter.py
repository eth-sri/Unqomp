from qiskit.circuit import Qubit, QuantumRegister
from qiskit.circuit.library import RCCXGate, IGate
from qiskit.tools.visualization import dag_drawer
from qiskit.dagcircuit import DAGCircuit
from unqomp.dependencygraph import DependencyGraph, Node, Edge, InitGate, DeallocateGate
from unqomp.ancillaallocation import AncillaRegister
from queue import PriorityQueue

class ConverterDependencyGraph:
    # maybe allow for extra custom gates (for which we'll need their inverse + ctrls/target + qfree or not), as arguments to ConverterDependencyGraph constructor
    known_qfree_gates = ['ccx', 'cnot', 'cx', 'i', 'id', 'iden', 'mct', 'mcx', 'mcx_gray', 'toffoli', 'x']
    known_non_qfree_gates = ['ch', 'crx', 'cry', 'crz', 'cu1', 'cu2', 'cu3', 'cy', 'cz', 'h', 'mcrx', 'mcry', 'mcrz', 'mcu1', 'r', 'rcccx', 'rccx', 'rx', 'ry', 'rz', 's', 'sdg', 't', 'tdg', 'u', 'u1', 'u2', 'u3', 'ucrx', 'ucry', 'ucrz', 'ucx', 'ucy', 'ucz', 'y', 'z']
    # left out on purpose: cswap, dcx, fredkin, iswap, mcmt, ms, rxx, ryy, rzx, rzz, swap, as those have two targets

    def __init__(self, extra_gates = []):
        # list of gates names to add to known_gates + bool if qfree (arguments have to be of the form (ctrl*, target))
        self.extra_qfree_gates = []
        self.extra_non_qfree_gates = []
        for (gate, is_qfree) in extra_gates:
            if is_qfree:
                self.extra_qfree_gates.append(gate.name)
            else:
                self.extra_non_qfree_gates.append(gate.name)
        pass
       

    def _isKnownInstruction(self, instruction):
        if instruction.name in self.extra_qfree_gates or instruction.name in self.extra_non_qfree_gates:
            return True
        if instruction.name in ConverterDependencyGraph.known_qfree_gates or instruction.name in ConverterDependencyGraph.known_non_qfree_gates:
            return True
        return False

    def _decomposeDAGToKnownGates(self, dag):
        unseen_gates = True
        while(unseen_gates):
            unseen_gates = False
            old_dag_nodes = dag.op_nodes()
            for node in old_dag_nodes:
                if not self._isKnownInstruction(node.op):
                    unseen_gates = True
                    if not self._decomposeNodeOnDAG(node, dag):
                        return None
        return dag

    def _simplUGates(self, dag):
        dag_nodes = dag.op_nodes()
        for node in dag_nodes:
            if node.op.name == 'u1' and node.op.params[0] == 0:
                node.op = IGate()
        for node in dag_nodes:
            if node.op.name == 'u' and node.op.params[0] == 0 and node.op.params[1] == 0 and node.op.params[2] == 0.0: #detects Id gates that have been implemented with u
                node.op = IGate()
        return dag

    def _decomposeNodeOnDAG(self, node, dag):
        # we cannot use qiskit.transpiler.passes.Decompose, since any custom gate is of type Instruction, and 
        # Decompose checks the type, so no way to decompose only those custom gates
        if not node.op.definition:
            print("could not deocmp " + str(node)+ " and op " + str(node.op.name))
            return False
        node_decomposition = DAGCircuit()
        qreg = set()
        creg = set()
        for instruction in node.op.definition:
            for qubit in instruction[1]:
                if not qubit.register in qreg:
                    node_decomposition.add_qreg(qubit.register)
                    qreg.add(qubit.register)
            for cbit in instruction[2]:
                 if not cbit.register in creg:
                    node_decomposition.add_creg(cbit.register)
                    creg.add(cbit.register)
        
        for instruction in node.op.definition:
            node_decomposition.apply_operation_back(*instruction)
        
        dag.substitute_node_with_dag(node, node_decomposition)
        return True

    def dagToDepGraph(self, dag):
        #We first decompose all the gates into gates we know how to deal with
        dag = self._decomposeDAGToKnownGates(dag)
        dag = self._simplUGates(dag)
        if dag is None:
            print("Could not decompose to known gates")
            return None

        dep_g = DependencyGraph(self.extra_qfree_gates, self.extra_non_qfree_gates)
        latest_node_on_wire = {}

        for dag_node in dag.topological_nodes():
            if dag_node.type == 'in':
                # this is the beginning of a wire, green node in dag drawings
                node = Node(dag_node.wire.register, dag_node.wire.index, InitGate(), 0, 0)
                latest_node_on_wire[node.variable_name] = node
                dep_g.addRootNode(node)
            elif dag_node.type == 'op':
                modified_qubit = dag_node.qargs[-1]
                wire_name = Node.wireName(modified_qubit.register, modified_qubit.index)
                assert self._isKnownInstruction(dag_node.op)
                assert latest_node_on_wire.get(wire_name) is not None
                previous_node_on_wire = latest_node_on_wire[wire_name]
                node = Node(modified_qubit.register, modified_qubit.index, dag_node.op, previous_node_on_wire.value_id + 1, 0)
                dep_g.addNode(node)
                dep_g.connectConsumedNodes(previous_node_on_wire, node)
                latest_node_on_wire[node.variable_name] = node
                for qarg in dag_node.qargs[:-1]: #assumes last element is always the target qubit
                    wire_name_dep = Node.wireName(qarg.register, qarg.index)
                    ctrl_node = latest_node_on_wire[wire_name_dep]
                    dep_g.connectDependencyNodes(ctrl_node, node)       
        return dep_g

    def depGraphToDag(self, dep_graph, registers_in_order):
        # first add the registers in the proper order
        dag = DAGCircuit()
        already_added = {} # registers may yield mutliple wires, we don't want to add them multiple times
        for qubit in registers_in_order:
            if already_added.get(qubit.register) is None:
                    already_added[qubit.register] = True
                    dag.add_qreg(qubit.register)
        for dep_node in dep_graph.nodesInTopologicalOrder():
            if dep_node.gate.name == 'init':
                if already_added.get(dep_node.register) is None:
                    already_added[dep_node.register] = True
                    dag.add_qreg(dep_node.register)
            else:
                #get the arguments of the gate: again assuming ctrls first, target last
                ctrl_reg = [Qubit(ctrl_edge.node_from.register, ctrl_edge.node_from.index) for ctrl_edge in dep_node.ctrl_edges_in]
                target_reg = Qubit(dep_node.consume_edge_in.node_from.register, dep_node.consume_edge_in.node_from.index)
                ctrl_reg.append(target_reg)
                dag.apply_operation_back(dep_node.gate, ctrl_reg)

        return dag

    def depGraphToDagWithLinking(self, dep_graph, registers_in_order):
        # insert deallocation nodes (to avoid linking a_1 to a_2 if there is a dependency edge between the 2)
        for var in dep_graph.nodes:
            list_nodes = dep_graph.nodes[var]
            if list_nodes and list_nodes[0]:
                cur_node = list_nodes[0][-1]
                if isinstance(cur_node.register, AncillaRegister):
                    if cur_node.consume_edge_out is None:
                        de_node = Node(cur_node.register, cur_node.index, DeallocateGate(), -1, 0)
                        dep_graph.addNode(de_node)
                        dep_graph.connectConsumedNodes(cur_node, de_node)

        # now the topo order and add registers to dag as they come:
        # we take nodes that have no parents (and remove them from the graph, giving new orphan nodes)
        # + we try to avoid taking ancillasFirstNodes as much as possible, and when we have to, we pick the ones with oldest date of birth
        not_ancilla_alloc = [] # parentless nodes that are not an ancilla alloc, simple list
        ancilla_alloc = PriorityQueue() #when pop: element with least prio comes

        # we first fill up both of those lists
        for var in dep_graph.nodes:
            # no need to look into the deallocate nodes, they all have a parent for now
            list_nodes = dep_graph.nodes[var]
            if list_nodes and list_nodes[0]:
                node = list_nodes[0][0]
                if node.consume_edge_in is None and not node.ctrl_edges_in:
                    if isinstance(node.register, AncillaRegister):
                        ancilla_alloc.put((node.register._allocation_date, node)) 
                    else:
                        not_ancilla_alloc.append(node)
        # now we go through the nodes in order, creating the dag and when possible reusing ancilla spots
        dag = DAGCircuit()
        already_added = {} # registers may yield mutliple wires, we don't want to add them multiple times

        # we add non ancilla registers already to have them in the right order
        for qubit in registers_in_order:
            if not isinstance(qubit.register, AncillaRegister):
                if already_added.get(qubit.register) is None:
                        already_added[qubit.register] = True
                        dag.add_qreg(qubit.register)

        available_ancilla_slots = [] # ancillas that have already been deallocated
        ancilla_correspondance = {} # a_c[a] = b if ancilla b can reuse ancilla a wire
        removed_edges = {} # for each node: remove_edges[node] = [nb_removed_consume_edges, nb_removed_ctrl_edges, nb_removed_non_Ctrl_edges]
        const_consume = 0
        const_ctrls = 1
        const_non_ctrls = 2
        # because we can't remove them really because they're needed to build the dag

        while not_ancilla_alloc or not ancilla_alloc.empty():
            cur_node = None
            # get the node, and deal with ancilla spots
            if not_ancilla_alloc:
                cur_node = not_ancilla_alloc.pop()
                if cur_node.gate.name == 'deallocate':
                    available_ancilla_slots.append(ancilla_correspondance[(cur_node.register, cur_node.index)])
            else:
                (allocation_date, cur_node) = ancilla_alloc.get()
                if available_ancilla_slots:
                    ancilla_correspondance[(cur_node.register, cur_node.index)] = available_ancilla_slots.pop()
                else:
                    # we don't want to keep ancilla registers as is, because they might have way too many qubits => new single qubits for them
                    anc_reg = QuantumRegister(1, name = "anc" + cur_node.register.name + str(cur_node.index))
                    already_added[anc_reg] = True
                    dag.add_qreg(anc_reg)
                    ancilla_correspondance[(cur_node.register, cur_node.index)] = (anc_reg, 0)
            
            # add node to dag
            if cur_node.gate.name == 'init':
                register = cur_node.register
                if ancilla_correspondance.get((cur_node.register, cur_node.index)) is not None:
                    (register, ind) = ancilla_correspondance[(cur_node.register, cur_node.index)]
                if already_added.get(register) is None:
                    already_added[register] = True
                    dag.add_qreg(register)
            elif cur_node.gate.name != 'deallocate':
                #get the arguments of the gate: again assuming ctrls first, target last
                # we have to replace ancilla registers
                ctrl_reg = [self._getQubit(ctrl_edge.node_from, ancilla_correspondance) for ctrl_edge in cur_node.ctrl_edges_in]
                target_reg = self._getQubit(cur_node.consume_edge_in.node_from, ancilla_correspondance)
                ctrl_reg.append(target_reg)
                # if gate is on an ancilla, all CCXs can be replaced by RCCXs, so we go recursively into the gate definition and replace all CCXs there
                if isinstance(cur_node.register, AncillaRegister):
                    cur_node.gate = _replaceCCXs(cur_node.gate)
                dag.apply_operation_back(cur_node.gate, ctrl_reg)

            # remove all outgoing edges from this node form dep_graph, and add new orphan nodes to lists
            if cur_node.consume_edge_out is not None:
                consuming_node = cur_node.consume_edge_out.node_to
                if removed_edges.get(consuming_node) is None:
                    removed_edges[consuming_node] = [0, 0, 0]
                removed_edges[consuming_node][const_consume] += 1
                if removed_edges[consuming_node][const_ctrls] == len(consuming_node.ctrl_edges_in) and removed_edges[consuming_node][const_non_ctrls] == len(consuming_node.non_ctrl_edges_in):
                    not_ancilla_alloc.append(consuming_node) # it consumes some node, so not ancilla alloc
            for e in cur_node.edges_out:
                node_to = e.node_to
                if removed_edges.get(node_to) is None:
                    removed_edges[node_to] = [0, 0, 0]
                if e.type == 'd':
                    removed_edges[node_to][const_ctrls] += 1
                else:
                    removed_edges[node_to][const_non_ctrls] += 1
                if removed_edges[node_to][const_ctrls] == len(node_to.ctrl_edges_in) and removed_edges[node_to][const_non_ctrls] == len(node_to.non_ctrl_edges_in):
                    if removed_edges[node_to][const_consume] == 1:
                        not_ancilla_alloc.append(node_to) # it consumes some node, so not ancilla alloc
                    elif node_to.consume_edge_in is None:
                        if isinstance(node_to.register, AncillaRegister):
                            ancilla_alloc.put((node_to.register._allocation_date, node_to))
                        else:
                            not_ancilla_alloc.append(node_to)
        return dag

    def _getQubit(self, node, ancilla_correspondance):
        if not isinstance(node.register, AncillaRegister):
            return Qubit(node.register, node.index)
        (reg, ind) = ancilla_correspondance[(node.register, node.index)]
        return Qubit(reg, ind)

def _replaceCCXs(gate):
        if gate.name == 'ccx':
            return RCCXGate()
        if gate.definition is None:
            return gate

        gate.definition._data = [(_replaceCCXs(inst), qargs, cargs)
                                         for inst, qargs, cargs in gate.definition]
        return gate

        
