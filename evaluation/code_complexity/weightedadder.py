# without unqomp, from https://github.com/Qiskit/qiskit-terra/blob/d7e469e33c212e8acf39308497bd312cfe792ca6/qiskit/circuit/library/arithmetic/weighted_adder.py
def WA():
        num_sum_qubits = int(np.floor(np.log2(sum(weights))) + 1) if sum(weights) > 0 else 1 
        num_result_qubits = num_state_qubits + num_sum_qubits
        num_carry_qubits = num_sum_qubits - 1
        num_control_qubits = num_sum_qubits > 2
        qr_state = self.qubits[:self.num_state_qubits]
        qr_sum = self.qubits[self.num_state_qubits:num_result_qubits]
        qr_carry = self.qubits[num_result_qubits:num_result_qubits + self.num_carry_qubits]
        qr_control = self.qubits[num_result_qubits + self.num_carry_qubits:]
        for i, weight in enumerate(self.weights):
            if np.isclose(weight, 0):
                continue
            q_state = qr_state[i]
            weight_binary = '{0:b}'.format(int(weight)).rjust(self.num_sum_qubits, '0')[::-1]
            for j, bit in enumerate(weight_binary):
                if bit == '1':
                    if self.num_sum_qubits == 1:
                        self.cx(q_state, qr_sum[j])
                    elif j == 0:
                        self.ccx(q_state, qr_sum[j], qr_carry[j])
                        self.cx(q_state, qr_sum[j])
                    elif j == self.num_sum_qubits - 1:
                        self.cx(q_state, qr_sum[j])
                        self.ccx(q_state, qr_carry[j - 1], qr_sum[j])
                    else:
                        self.x(qr_sum[j])
                        self.x(qr_carry[j - 1])
                        self.mct([q_state, qr_sum[j], qr_carry[j - 1]], qr_carry[j], qr_control)
                        self.cx(q_state, qr_carry[j])
                        self.x(qr_sum[j])
                        self.x(qr_carry[j - 1])
                        self.cx(q_state, qr_sum[j])
                        self.ccx(q_state, qr_carry[j - 1], qr_sum[j])
                else:
                    if self.num_sum_qubits == 1:
                        pass  # nothing to do, since nothing to add
                    elif j == 0:
                        pass  # nothing to do, since nothing to add
                    elif j == self.num_sum_qubits-1:
                        self.ccx(q_state, qr_carry[j - 1], qr_sum[j])
                    else:
                        self.mct([q_state, qr_sum[j], qr_carry[j - 1]], qr_carry[j], qr_control)
                        self.ccx(q_state, qr_carry[j - 1], qr_sum[j])
            for j in reversed(range(len(weight_binary))):
                bit = weight_binary[j]
                if bit == '1':
                    if self.num_sum_qubits == 1:
                        pass
                    elif j == 0:
                        self.x(qr_sum[j])
                        self.ccx(q_state, qr_sum[j], qr_carry[j])
                        self.x(qr_sum[j])
                    elif j == self.num_sum_qubits - 1:
                        pass
                    else:
                        self.x(qr_carry[j - 1])
                        self.mct([q_state, qr_sum[j], qr_carry[j - 1]], qr_carry[j], qr_control)
                        self.cx(q_state, qr_carry[j])
                        self.x(qr_carry[j - 1])
                else:
                    if self.num_sum_qubits == 1:
                        pass
                    elif j == 0:
                        pass
                    elif j == self.num_sum_qubits - 1:
                        pass
                    else:
                        self.x(qr_sum[j])
                        self.mct([q_state, qr_sum[j], qr_carry[j - 1]], qr_carry[j], qr_control)
                        self.x(qr_sum[j])

# with unqomp
def makeWeightedAdder(num_state_qubits, weights):
    num_sum_qubits = int(np.floor(np.log2(sum(weights))) + 1) if sum(weights) > 0 else 1 
    num_carry_qubits = num_sum_qubits -1
    for i, weight in enumerate(weights):
        if not np.isclose(weight, np.round(weight)):
            raise ValueError('Non-integer weights are not supported!')
        weights[i] = np.round(weight)
    qr_state = QuantumRegister(num_state_qubits, name = 'state')
    qr_sum = QuantumRegister(num_sum_qubits, name = 'sum')
    circuit = AncillaCircuit(qr_state, qr_sum)
    for i, weight in enumerate(weights):
        if np.isclose(weight, 0):
            continue
        q_state = qr_state[i]
        weight_binary = '{0:b}'.format(int(weight)).rjust(num_sum_qubits, '0')[::-1]
        qr_carry = circuit.new_ancilla_register(num_carry_qubits, name = "a" + str(i))
        for j, bit in enumerate(weight_binary):
            if bit == '1':
                if num_sum_qubits == 1:
                    circuit.cx(q_state, qr_sum[j])
                elif j == 0:
                    circuit.ccx(q_state, qr_sum[j], qr_carry[j])
                    circuit.cx(q_state, qr_sum[j])
                elif j == num_sum_qubits - 1:
                    circuit.cx(q_state, qr_sum[j])
                    circuit.ccx(q_state, qr_carry[j - 1], qr_sum[j])
                else:
                    circuit.mcx([q_state, qr_sum[j], qr_carry[j - 1]], qr_carry[j], negated_ctrls = [1, 2])
                    circuit.cx(q_state, qr_carry[j])
                    circuit.cx(q_state, qr_sum[j])
                    circuit.ccx(q_state, qr_carry[j - 1], qr_sum[j])
            else:
                if num_sum_qubits == 1:
                    pass  # nothing to do, since nothing to add
                elif j == 0:
                    pass  # nothing to do, since nothing to add
                elif j == num_sum_qubits-1:
                    circuit.ccx(q_state, qr_carry[j - 1], qr_sum[j])
                else:
                    circuit.mcx([q_state, qr_sum[j], qr_carry[j - 1]], qr_carry[j])
                    circuit.ccx(q_state, qr_carry[j - 1], qr_sum[j])
    return circuit
