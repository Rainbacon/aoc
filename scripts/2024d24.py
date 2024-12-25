import os

literals = {}
gates = {}
with open("data/2024/24/input.txt") as f:
    i = f.read()
    lines = i.split("\n\n")
    for line in lines[0].split("\n"):
        parts = line.split(": ")
        literals[parts[0]] = parts[1]
    
    for line in lines[1].split("\n"):
        parts = line.split(" -> ")
        gates[parts[1]] = parts[0].split(" ")

def find_gate(pred):
    for k,v in gates.items():
        if pred(v):
            return k

def combo(gate, in1, in2):
    pred1 = gate[0] == in1 and gate[2] == in2
    pred2 = gate[0] == in2 and gate[2] == in1
    return pred1 or pred2

adders = []
adders.append({
    "inputs": ["x00", "y00"],
    "output": "z00",
    "carry": find_gate(lambda g: g[1] == "AND" and combo(g, "x00", "y00"))
})

for i in range(1, 45):
    in1 = "x%02d" % i
    in2 = "y%02d" % i
    out = "z%02d" % i
    intermediateAdd = find_gate(lambda g: g[1] == "XOR" and combo(g, in1, in2))
    intermediateCarry = find_gate(lambda g: g[1] == "AND" and combo(g, in1, in2))
    outputGate = gates[out]
    prevCarry = adders[i-1]["carry"]
    if not combo(outputGate, prevCarry, intermediateAdd)