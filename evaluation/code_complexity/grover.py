# without unqomp, from https://github.com/Qiskit/qiskit-aqua/blob/13b31d5f91dbb58d865c7e5277a23e59010fd8c5/qiskit/aqua/algorithms/amplitude_amplifiers/grover.py
def GroverQiskit(oracle): 
    _num_iterations = int(np.floor(np.pi / 4.0 * np.sqrt(pow(2, n))))
    def _construct_diffusion_circuit():
        qc = QuantumCircuit(_oracle.variable_register)
        num_variable_qubits = len(_oracle.variable_register)
        num_ancillae_needed = max(0, num_variable_qubits - 2)
        num_oracle_ancillae = \
            len(_oracle.ancillary_register) if _oracle.ancillary_register else 0
        num_additional_ancillae = num_ancillae_needed - num_oracle_ancillae
        if num_additional_ancillae > 0:
            extra_ancillae = QuantumRegister(num_additional_ancillae, name='a_e')
            qc.add_register(extra_ancillae)
            ancilla = list(extra_ancillae)
            if num_oracle_ancillae > 0:
                ancilla += list(_oracle.ancillary_register)
        else:
            ancilla = _oracle.ancillary_register
        if _oracle.ancillary_register:
            qc.add_register(_oracle.ancillary_register)
        qc.h(_oracle.output_register)
        qc.u(np.pi, 0, np.pi, _oracle.variable_register)
        qc.u(np.pi/2, 0, np.pi, _oracle.variable_register[num_variable_qubits - 1])
        qc.mct(
            _oracle.variable_register[0:num_variable_qubits - 1],
            _oracle.variable_register[num_variable_qubits - 1],
            ancilla,
            mode='v-chain'
        )
        qc.u(np.pi/2, 0, np.pi, _oracle.variable_register[num_variable_qubits - 1])
        qc.u(np.pi, 0, np.pi, _oracle.variable_register)
        qc.h(_oracle.output_register)
        return qc
    def qc_amplitude_amplification_iteration():
        _qc_aa_iteration = QuantumCircuit()
        _qc_aa_iteration += _oracle.circuit
        _qc_aa_iteration += _construct_diffusion_circuit()
        return _qc_aa_iteration
    _qc_amplitude_amplification = QuantumCircuit()
    for _ in range(_num_iterations):
        _qc_amplitude_amplification += qc_amplitude_amplification_iteration()
    qc = QuantumCircuit(_oracle.variable_register, _oracle.output_register)
    qc.u(np.pi, 0, np.pi, _oracle.output_register)  # x
    qc.u(np.pi/2, 0, np.pi, _oracle.output_register)  # h
    qc.h(_oracle.output_register)
    qc += _qc_amplitude_amplification
    return qc

#with unqomp
def GroverQpp(n, oracle):
    _num_iterations = int(np.floor(np.pi / 4.0 * np.sqrt(pow(2, n))))
    def _construct_diffusion_circuit():
        var_reg2 = QuantumRegister(n)
        qc = AncillaCircuit(var_reg2)
        qc.h(var_reg2)
        qc.u(np.pi, 0, np.pi, var_reg2)
        qc.u(np.pi/2, 0, np.pi, var_reg2[-1])
        qc.mcx(
            var_reg2[:-1], 
            var_reg2[-1]
        )
        qc.u(np.pi/2, 0, np.pi, var_reg2[-1])
        qc.u(np.pi, 0, np.pi, var_reg2)
        qc.h(var_reg2)
        return qc
    def qc_amplitude_amplification_iteration():
        reg = QuantumRegister(n+1, name = 'reg')
        _qc_aa_iteration = AncillaCircuit(reg)
        _qc_aa_iteration.append(oracle, reg)
        _qc_aa_iteration.append(_construct_diffusion_circuit().to_ancilla_gate(), reg[:-1])
        return _qc_aa_iteration
    var_reg = QuantumRegister(n, name = 'var_reg')
    out_reg = QuantumRegister(1, name= 'out_reg')
    qc = AncillaCircuit(var_reg, out_reg)
    qc.u(np.pi, 0, np.pi, out_reg)  # x
    qc.u(np.pi/2, 0, np.pi, out_reg)  # h
    qc.h(var_reg)
    for _ in range(_num_iterations):
        qc.append(qc_amplitude_amplification_iteration().to_ancilla_gate(), [*var_reg, out_reg])
    return (qc, var_reg)
