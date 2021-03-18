# Unqomp

Unqomp is a procedure to automatically synthesize uncomputation in a given
quantum circuit.

This repository integrates Unqomp into Qiskit, allowing the programmer to mark
quantum bits as ancillas, which are then safely uncomputed by Unqomp.

## Getting Started Guide

### Installation

You can install this project using [pip](https://pypi.org/project/pip/). For
example, to install via [conda](https://docs.conda.io/en/latest/), use

```bash
conda create --name unqomp --yes python=3.8
conda activate unqomp
pip install .
```

For development of Unqomp, install it in editable mode as follows:

```bash
pip install -e .
```

Note that Unqomp was developed for Qiskit 0.22.0, and may not work with later
 versions.

## Start Programming

The following code snippet creates a simple adder circuit:

```python
from qiskit import QuantumRegister
from unqomp.ancillaallocation import AncillaRegister, AncillaCircuit

x = QuantumRegister(1, name = 'x')
y = QuantumRegister(1, name = 'y')
b = QuantumRegister(1, name = 'b')
c = AncillaRegister(1, name = 'c')

circ = AncillaCircuit(x, y, b, c) # ancillas should always come last
circ.ccx(b, x, c) # all gates can be used on an AncillaCircuit directly
circ.cx(b, x)
circ.cx(c, y)

print(circ) # AncillaCircuit contains no uncomputation

# Output:
#           ┌───┐
# x_0: ──■──┤ X ├─────
#        │  └─┬─┘┌───┐
# y_0: ──┼────┼──┤ X ├
#        │    │  └─┬─┘
# b_0: ──■────■────┼──
#      ┌─┴─┐       │  
# c_0: ┤ X ├───────■──
#      └───┘

# Uncomputation is added, ancillas allocated and uncomputed CCX gates are replaced by the Margolus RCCX gate. The AncillaCircuit is converted to a qiskit QuantumCircuit, which does not distinguish ancillas from other qubits.
circ = circ.circuitWithUncomputation()

print(circ)

# Output:
#          ┌───────┐     ┌───────┐┌───┐
#     x_0: ┤1      ├─────┤1      ├┤ X ├
#          │       │┌───┐│       │└─┬─┘
#     y_0: ┤       ├┤ X ├┤       ├──┼──
#          │  RCCX │└─┬─┘│  RCCX │  │  
#     b_0: ┤0      ├──┼──┤0      ├──■──
#          │       │  │  │       │
# ancc0_0: ┤2      ├──■──┤2      ├─────
#          └───────┘     └───────┘
```

### Composing circuits

To compose AncillaCircuits, they can be transformed into an AncillaGate. When appending such a gate to an AncillaCircuit, all needed ancillas are appended automatically.

```python
from qiskit import QuantumRegister
from unqomp.ancillaallocation import AncillaRegister, AncillaCircuit

x = QuantumRegister(1, name = 'x')
y = QuantumRegister(1, name = 'y')
b = QuantumRegister(1, name = 'b')
c = AncillaRegister(1, name = 'c')

circ = AncillaCircuit(x, y, b, c) # ancillas should always come last
circ.ccx(b, x, c)
circ.cx(b, x)
circ.cx(c, y)

gate = circ.to_ancilla_gate()

z = QuantumRegister(1, name = 'z')
t = QuantumRegister(1, name = 't')
d = QuantumRegister(1, name = 'd')
circ2 = AncillaCircuit(z, t, d)

# append gate to circ2, adding d to (z, t), the ancilla c is appended automatically
circ2.append(gate, [z, t, d])
# appending it again, a new ancilla is added again
circ2.append(gate, [z, t, d])

print(circ2)

# Output:
#       ┌───────────┐┌───────────┐
#  z_0: ┤0          ├┤0          ├
#       │           ││           │
#  t_0: ┤1          ├┤1          ├
#       │  circuit7 ││           │
#  d_0: ┤2          ├┤2 circuit7 ├
#       │           ││           │
# q0_0: ┤3          ├┤           ├
#       └───────────┘│           │
# q1_0: ─────────────┤3          ├
#                    └───────────┘

circ2 = circ2.circuitWithUncomputation() # the two ancillas are now allocated on the same qubit

print(circ2)

# Output:
#           ┌───────┐     ┌───────┐┌───┐┌───────┐     ┌───────┐┌───┐
#      z_0: ┤1      ├─────┤1      ├┤ X ├┤1      ├─────┤1      ├┤ X ├
#           │       │┌───┐│       │└─┬─┘│       │┌───┐│       │└─┬─┘
#      t_0: ┤       ├┤ X ├┤       ├──┼──┤       ├┤ X ├┤       ├──┼──
#           │  RCCX │└─┬─┘│  RCCX │  │  │  RCCX │└─┬─┘│  RCCX │  │  
#      d_0: ┤0      ├──┼──┤0      ├──■──┤0      ├──┼──┤0      ├──■──
#           │       │  │  │       │     │       │  │  │       │
# ancq00_0: ┤2      ├──■──┤2      ├─────┤2      ├──■──┤2      ├─────
#           └───────┘     └───────┘     └───────┘     └───────┘
```

## Step-by-Step Instuctions to Reproduce Evaluation

In the following, we describe how to reproduce the evaluation results from
PLDI'21 paper "Unqomp: Synthesizing Uncomputation in Quantum Circuits".

### Organization

The Unqomp implementation is made of the files
[unqomp/ancillaallocation.py](unqomp/ancillaallocation.py),
[unqomp/converter.py](unqomp/converter.py),
[unqomp/dependencygraph.py](unqomp/dependencygraph.py) and
[unqomp/uncomputation.py](unqomp/uncomputation.py).

All examples presented in the submitted paper are implemented using Unqomp and
can be found in [unqomp/examples/](unqomp/examples/).

### Paper Claims

#### Q1: Code length

Links to the original Qiskit and Cirq implementations of all the examples can be
found in [evaluation/code_complexity](code_complexity). Each file also contains the
relevant parts of the Unqomp and original implementations, which we used to manually
 count the number of lines in each implementation.

#### Q2: Efficiency

Running [run_evaluation.py](run_evaluation.py) outputs the relative numbers of
qubits and gates as shown in Table 2 of our publication. To output the absolute
values of qubits and gates as shown in Table 4 in the appendix, pass the argument
`--absolute`.

Running [plots.py](plots.py) creates the csv files used to generate the plots in
Fig.9, in [evaluation/plot_values/](plot_values/). As this requires a long run, by default
only a few values are computed. To compute all values, pass the argument
`--all`. The csv files already present in [evaluation/plot_values/](plot_values/) contain
all expected values.
