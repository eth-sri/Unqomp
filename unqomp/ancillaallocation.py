from qiskit.circuit import Qubit, QuantumRegister, QuantumCircuit, Gate
from itertools import count

allocation_time_counter = count()

class AncillaRegister(QuantumRegister):
    def __init__(self, nb_qubits, name = None):
        if name:
            QuantumRegister.__init__(self, nb_qubits, name)
        else:
            QuantumRegister.__init__(self, nb_qubits)
        self._allocation_date = next(allocation_time_counter)

class AncillaGate:
    def __init__(self, gate, nb_ancillas = 0, extra_qfree = []):
        self._gate = gate
        self._nb_ancillas = nb_ancillas
        self._extra_qfree = extra_qfree

class AncillaCircuit(QuantumCircuit):
    # Mostly delegates to circuit, except for mcx, mcry where we use our custom implementation
    # plus allocates ancillas for gates
    def __init__(self, *regs, name = None):
        self._nb_ancillas = 0
        self._extra_qfree_gates = [] # records custom gates to consider qfree when uncomputing
        self._ancillas_list = []
        if isinstance(regs[0], int):
            QuantumCircuit.__init__(self, regs[0], name = name)
        elif isinstance(regs[0], AncillaRegister):
            QuantumCircuit.__init__(self, regs[0], name = name)
            self._nb_ancillas += len(regs[0][:])
            self._ancillas_list.append(regs[0])
        else:
            QuantumCircuit.__init__(self, regs[0], name = name)
        for reg in regs[1:]:
            self.add_register(reg)

    def append(self, instruction, qargs = None, cargs = None):
        if isinstance(instruction, AncillaGate) and instruction._nb_ancillas > 0:
            anc = AncillaRegister(instruction._nb_ancillas)
            self.add_register(anc) # updates nb ancillas
            assert cargs is None
            for qf in instruction._extra_qfree:
                if not qf in self._extra_qfree_gates:
                    self._extra_qfree_gates.append(qf)
            QuantumCircuit.append(self, instruction._gate, [*qargs, *anc[:]])
        elif isinstance(instruction, AncillaGate):
            for qf in instruction._extra_qfree:
                if not qf in self._extra_qfree_gates:
                    self._extra_qfree_gates.append(qf)
            QuantumCircuit.append(self, instruction._gate, qargs)
        else:
            QuantumCircuit.append(self, instruction, qargs, cargs)

    def add_register(self, reg):
        QuantumCircuit.add_register(self, reg)
        if isinstance(reg, AncillaRegister):
            self._ancillas_list.append(reg)
            self._nb_ancillas += len(reg[:])

    def new_ancilla_register(self, num_qubits, name = None):
        a = AncillaRegister(num_qubits, name)
        self.add_register(a)
        return a

    def to_ancilla_gate(self, is_qfree = False):
        # self should have registers in the following order: first ctrls, then target then ancillas
        gate = self.to_gate()
        extra_qfree_gates = [gate] if is_qfree else self._extra_qfree_gates
        return AncillaGate(gate, self._nb_ancillas, extra_qfree_gates)

    def addQfreeGate(self, gate):
        self._extra_qfree_gates.append(gate)

    def circuitWithUncomputation(self):
        from unqomp.uncomputation import uncomputeAllAncillas
        return uncomputeAllAncillas(self, [(gate, True) for gate in self._extra_qfree_gates])

    def mcx(self, ctrls, target, negated_ctrls = []): #allows for negated ctrls
        from unqomp.examples.mcx import makeMCX
        def makeNegatedMCXGate(num_ctrls):
            ctrls = QuantumRegister(num_ctrls)
            target = QuantumRegister(1)
            if num_ctrls <= 2:
                circuit = AncillaCircuit(ctrls, target)
                for i in negated_ctrls:
                    circuit.x(ctrls[i])
                if num_ctrls == 2:
                    circuit.ccx(ctrls[0], ctrls[1], target)
                elif num_ctrls == 1:
                    circuit.cx(ctrls[0], target)
                for i in negated_ctrls:
                    circuit.x(ctrls[i])
                return circuit.to_ancilla_gate(True)
            mcx_gate = makeMCX(n)
            circuit = AncillaCircuit(ctrls)
            anc = circuit.new_ancilla_register(mcx_gate._nb_ancillas)
            circuit.add_register(target)
            for i in negated_ctrls:
                circuit.x(ctrls[i])
            mcx_gate_w_uncomp = mcx_gate.circuitWithUncomputation().to_gate()
            circuit.append(mcx_gate_w_uncomp, [*ctrls[:], target, *anc[:]])
            for i in negated_ctrls:
                circuit.x(ctrls[i])
            return circuit.to_ancilla_gate(True)

        n = len(ctrls[:])
        mcx_gate = None
        if len(negated_ctrls) > 0:
            mcx_gate = makeNegatedMCXGate(n)
            if mcx_gate._nb_ancillas > 0:
                anc = self.new_ancilla_register(mcx_gate._nb_ancillas)
                self.addQfreeGate(mcx_gate._gate)
                QuantumCircuit.append(self, mcx_gate._gate, [*ctrls[:], *anc[:], target])
        else:
            mcx_gate = makeMCX(n).to_ancilla_gate()
            self.append(mcx_gate, [*ctrls[:], target])

    def mcry(self, rot_coeff, ctrls, target):
        from unqomp.examples.mcx import makeMCRY
        n = len(ctrls[:])
        mcry_gate = makeMCRY(rot_coeff, n).to_ancilla_gate()
        self.append(mcry_gate, [*ctrls[:], target])

    
