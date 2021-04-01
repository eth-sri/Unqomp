#without unqomp, from https://github.com/Qiskit/qiskit-aqua/blob/13b31d5f91dbb58d865c7e5277a23e59010fd8c5/qiskit/aqua/algorithms/education/deutsch_jozsa.py
def makesDJ(num_qubits):
		var_reg = QuantumRegister(num_qubits, name = 'vals')
    out_reg = QuantumRegister(1, name = 'out')
    qc_preoracle = QuantumCircuit(.variable_register, .output_register)
    qc_preoracle.h(self._oracle.variable_register)
    qc_preoracle.x(self._oracle.output_register)
    qc_preoracle.h(self._oracle.output_register)
    qc_preoracle.barrier()
    qc_oracle = self._oracle.circuit
    qc_postoracle = QuantumCircuit(variable_register,output_register)
    qc_postoracle.h(self._oracle.variable_register)
    qc_postoracle.barrier()
    self._circuit = qc_preoracle + qc_oracle + qc_postoracle
#with unqomp
def makesDJ():
		var_reg = QuantumRegister(num_qubits, name = 'vals')
    out_reg = QuantumRegister(1, name = 'out')
    circ = AncillaCircuit(var_reg, out_reg)
    circ.h(var_reg)
    circ.x(out_reg)
    circ.h(out_reg)
		circ.barrier()
    circ.append(oracle_gate, [*var_reg, out_reg[0]])
    circ.h(var_reg)
		circ.barrier()
    return (circ, var_reg)