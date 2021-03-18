# from https://github.com/Qiskit/qiskit-terra/blob/d7e469e33c212e8acf39308497bd312cfe792ca6/qiskit/circuit/library/arithmetic/integer_comparator.py
def IntegerComparator():
        qr_state = self.qubits[:self.num_state_qubits]
        q_compare = self.qubits[self.num_state_qubits]
        qr_ancilla = self.qubits[self.num_state_qubits + 1:]
        if self.value <= 0:  # condition always satisfied for non-positive values
            if self._geq:  # otherwise the condition is never satisfied
                self.x(q_compare)
        elif self.value < pow(2, self.num_state_qubits):
            if self.num_state_qubits > 1:
                twos = self._get_twos_complement()
                for i in range(self.num_state_qubits):
                    if i == 0:
                        if twos[i] == 1:
                            self.cx(qr_state[i], qr_ancilla[i])
                    elif i < self.num_state_qubits - 1:
                        if twos[i] == 1:
                            self.compose(OR(2), [qr_state[i], qr_ancilla[i - 1], qr_ancilla[i]],
                                         inplace=True)
                        else:
                            self.ccx(qr_state[i], qr_ancilla[i - 1], qr_ancilla[i])
                    else:
                        if twos[i] == 1:
                            self.compose(OR(2), [qr_state[i], qr_ancilla[i - 1], q_compare],
                                         inplace=True)
                        else:
                            self.ccx(qr_state[i], qr_ancilla[i - 1], q_compare)
                if not self._geq:
                    self.x(q_compare)
                for i in reversed(range(self.num_state_qubits-1)):
                    if i == 0:
                        if twos[i] == 1:
                            self.cx(qr_state[i], qr_ancilla[i])
                    else:
                        if twos[i] == 1:
                            self.compose(OR(2), [qr_state[i], qr_ancilla[i - 1], qr_ancilla[i]],
                                         inplace=True)
                        else:
                            self.ccx(qr_state[i], qr_ancilla[i - 1], qr_ancilla[i])
            else:
                self.cx(qr_state[0], q_compare)
                if not self._geq:
                    self.x(q_compare)
        else:
            if not self._geq:  # otherwise the condition is never satisfied
                self.x(q_compare)

# with uncomp
def makeIntegerComparator(num_state_qubits, value, geq  = True, with_uncomp = True):
    or_gate = OR(2).to_gate()
    qr_state = QuantumRegister(num_state_qubits, name='state')
    q_compare = QuantumRegister(1, name='compare')
    circuit = AncillaCircuit(qr_state, q_compare)
    circuit.addQfreeGate(or_gate)
    qr_ancilla = circuit.new_ancilla_register(num_state_qubits - 1, name = 'ancilla') if num_state_qubits > 1 else None
    if value <= 0:  # condition always satisfied for non-positive values
        if geq:  # otherwise the condition is never satisfied
            circuit.x(q_compare)
    # condition never satisfied for values larger than or equal to 2^n
    elif value < pow(2, num_state_qubits):
        if num_state_qubits > 1:
            twos = _get_twos_complement(num_state_qubits, value)
            for i in range(num_state_qubits):
                if i == 0:
                    if twos[i] == 1:
                        circuit.cx(qr_state[i], qr_ancilla[i])
                elif i < num_state_qubits - 1:
                    if twos[i] == 1:
                        circuit.append(or_gate, [qr_state[i], qr_ancilla[i - 1], qr_ancilla[i]])
                    else:
                        circuit.ccx(qr_state[i], qr_ancilla[i - 1], qr_ancilla[i])
                else:
                    if twos[i] == 1:
                        circuit.append(or_gate, [qr_state[i], qr_ancilla[i - 1], q_compare])
                    else:
                        circuit.ccx(qr_state[i], qr_ancilla[i - 1], q_compare)
            if not geq:
                circuit.x(q_compare)
        else:
           circuit.cx(qr_state[0], q_compare)
            if not geq:
                circuit.x(q_compare)
    else:
        if not geq:  # otherwise the condition is never satisfied
            circuit.x(q_compare)
    return circuit
