class InitGate:
    def __init__(self):
        self.name = 'init'

class DeallocateGate:
    def __init__(self):
        self.name = 'deallocate'

class Edge:
    def __init__(self, node_from, node_to, edge_type):
        self.node_from = node_from
        self.node_to = node_to
        self.type = edge_type

    def __repr__(self):
        return "Edge: from({}), to({}), type({})".format(self.node_from, self.node_to, self.type)

class Node:
    def __init__(self, register, index, gate, value_id = 0, copy_id = 0):
        self.register = register
        self.index = index
        self.variable_name = Node.wireName(register, index)
        self.gate = gate
        self.value_id = value_id
        self.copy_id = copy_id
        self.ctrl_edges_in = []
        self.non_ctrl_edges_in = []
        self.edges_out = []
        self.consume_edge_in = None
        self.consume_edge_out = None

    def __repr__(self):
        return "Node: var({}), vId({}), cId({})".format(self.variable_name, self.value_id, self.copy_id)

    def __gt__(self, node2): #order does not matter so much, we just want to break ties when comparing ancillaRegisters
        if self.register.name == node2.register.name:
            return self.index > node2.index
        return self.register.name > node2.register.name

    def wireName(register, index):
        return "%s[%s]" % (register.name, index)

class DependencyGraph:
    def __init__(self, extra_qfree_gates = [], extra_non_qfree_gates = []):
        self.nodes = {} #for a variable name, contains a list (ordered by value_id) of list (ordered by copy_id) of nodes
        self.deallocate_nodes = [] # we don't want to pollute the graph to much
        self.extra_qfree_gates = extra_qfree_gates
        self.extra_non_qfree_gates = extra_non_qfree_gates

    def addNode(self, node):
        list_nodes = self.nodes.get(node.variable_name)
        if node.gate.name == 'deallocate':
            self.deallocate_nodes.append(node)
            return
        if list_nodes is not None:
            if len(list_nodes) > node.value_id:
                assert len(list_nodes[node.value_id]) == node.copy_id
                list_nodes[node.value_id].append(node)
            else:
                assert len(list_nodes) == node.value_id and node.copy_id == 0
                list_nodes.append([node])
        else:
            assert node.value_id == 0 and node.copy_id == 0
            self.nodes[node.variable_name] = [[node]]

    def addRootNode(self, node):
        self.addNode(node)        

    def connectConsumedNodes(self, node_from, node_to, return_new_edges = False):
        assert node_from.variable_name == node_to.variable_name
        assert node_from.value_id == node_to.value_id + 1 or node_from.value_id == node_to.value_id - 1 #computation or uncomputation
        assert node_to.consume_edge_in is None
        assert node_from.consume_edge_out is None
        # removed bc of deallocate nodes, without them holds
        #assert self.nodes[node_from.variable_name][node_from.value_id][node_from.copy_id] == node_from
        #assert self.nodes[node_to.variable_name][node_to.value_id][node_to.copy_id] == node_to
        e = Edge(node_from, node_to, 'c')
        node_from.consume_edge_out = e
        node_to.consume_edge_in = e
        added_avail_edges = self._updatesAvailabilityEdges(e, return_new_edges)
        if(return_new_edges):
            added_avail_edges.append(e)
            return added_avail_edges
    
    def connectDependencyNodes(self, node_from, node_to, return_new_edges = False):
        assert node_from.variable_name != node_to.variable_name
        assert self.nodes[node_from.variable_name][node_from.value_id][node_from.copy_id] == node_from
        assert self.nodes[node_to.variable_name][node_to.value_id][node_to.copy_id] == node_to
        e = Edge(node_from, node_to, 'd')
        node_from.edges_out.append(e)
        node_to.ctrl_edges_in.append(e)
        added_avail_edges = self._updatesAvailabilityEdges(e, return_new_edges)
        if(return_new_edges):
            added_avail_edges.append(e)
            return added_avail_edges

    def _connectAvailabilityNodes(self, node_from, node_to, return_new_edges = False):
        assert node_from.variable_name != node_to.variable_name
        # removed bc of deallocate gates, holds otherwise
        #assert self.nodes[node_from.variable_name][node_from.value_id][node_from.copy_id] == node_from
        #assert self.nodes[node_to.variable_name][node_to.value_id][node_to.copy_id] == node_to
        e = Edge(node_from, node_to, 'a')
        node_from.edges_out.append(e)
        node_to.non_ctrl_edges_in.append(e)
        if return_new_edges:
            return e
    
    def connectUncomputations(self, node_from, node_to, return_new_edges = False):
        assert self.nodes[node_from.variable_name][node_from.value_id][node_from.copy_id] == node_from
        assert self.nodes[node_to.variable_name][node_to.value_id][node_to.copy_id] == node_to
        e = Edge(node_from, node_to, 'u')
        node_from.edges_out.append(e)
        node_to.non_ctrl_edges_in.append(e)
        if return_new_edges:
            return e

    def _updatesAvailabilityEdges(self, edge, return_new_edges = False):
        new_edges = []
        if edge.type == 'c':
            #edge is some x1 -> x2, for all dependency edges x1 -> y, we add the availability one y -> x2
            for edge_out in edge.node_from.edges_out:
                if edge_out.type == 'd':
                    e = self._connectAvailabilityNodes(edge_out.node_to, edge.node_to, return_new_edges)
                    if return_new_edges:
                        new_edges.append(e)
        elif edge.type == 'd':
            #edge is some x1 -> y, if there is some consume edge x1 -> x2, we add the availability one y -> x2
            if edge.node_from.consume_edge_out is not None:
                e = self._connectAvailabilityNodes(edge.node_to, edge.node_from.consume_edge_out.node_to, return_new_edges)
                if return_new_edges:
                        new_edges.append(e)
        
        if return_new_edges:
            return new_edges

    def _aux_topoligical_sort(self, sorted_nodes, seen_nodes, node):
        seen = seen_nodes.get(node)
        if seen is not None:
            if seen == 1:
                print("wow cycle from node")
                print(node)
                assert False
            else:
                return
        seen_nodes[node] = 1 #to detect cycle, changed to 2 when the node is not active anymore
        for e in node.edges_out:
            self._aux_topoligical_sort(sorted_nodes, seen_nodes, e.node_to)
        if node.consume_edge_out is not None:
            self._aux_topoligical_sort(sorted_nodes, seen_nodes, node.consume_edge_out.node_to)
        seen_nodes[node] = 2
        sorted_nodes.append(node)

    def nodesInTopologicalOrder(self):
        sorted_nodes = []
        seen_nodes = {}
        for variable_name in self.nodes:
            for list_copies in self.nodes[variable_name]:
                for node in list_copies:
                    self._aux_topoligical_sort(sorted_nodes, seen_nodes, node)
        sorted_nodes.reverse()
        return sorted_nodes

    def removeEdge(self, edge):
        # assumes the availability edges that were created are taken care of, we don't touch them here
        if edge.type == 'c':
            edge.node_from.consume_edge_out = None
            edge.node_to.consume_edge_in = None
        elif edge.type == 'd':
            edge.node_from.edges_out.remove(edge)
            edge.node_to.ctrl_edges_in.remove(edge)
        else:
            edge.node_from.edges_out.remove(edge)
            edge.node_to.non_ctrl_edges_in.remove(edge)

    def removeNode(self, node):
        #we can only remove the latest node on a wire
        assert node.consume_edge_in is None
        assert node.consume_edge_out is None
        assert not node.edges_out
        assert not node.ctrl_edges_in
        assert not node.non_ctrl_edges_in
        assert self.nodes.get(node.variable_name) is not None
        assert len(self.nodes[node.variable_name]) > node.value_id
        assert len(self.nodes[node.variable_name][node.value_id]) == node.copy_id + 1
        self.nodes[node.variable_name][node.value_id].pop()

    def latestCopy(self, node):
        assert self.nodes[node.variable_name] is not None and len(self.nodes[node.variable_name]) > node.value_id
        return self.nodes[node.variable_name][node.value_id][-1]

    def hasCycleWith(self, orig_node):
        #returns true if there is a cycle containing orig_node
        marked = {} # 1 if seen, 2 if active

        def hasCycle(node):
            t = marked.get(node)
            if t is not None:
                if t==2:
                    return True
                else:
                    return False
            marked[node] = 2
            if node.consume_edge_out and hasCycle(node.consume_edge_out.node_to):
                return True
            for e in node.edges_out:
                if hasCycle(e.node_to):
                    return True
            marked[node] = 1
            return False

        return hasCycle(orig_node)


                        

