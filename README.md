# rao
Independent verification of the classification of tilings by convex pentagons.

## Paper
Michael Rao's paper [Exhaustive search of convex pentagons which tile the plane][paper:rao] contains a proof that the classification of [tilings by convex pentagons][wikipedia:pentagonal_tiling] is complete.

The paper is centered around two algorithms, with accompanying definitions, theorems and proofs.

## Goal
The goal for this repository is to develop an independent verification of Rao's result.

### Algorithm 1

```plain
procedure Recurse(X)
  X ← Compat(X)
  if P^≥_X ∩]0, 1[ = ∅ then 
    return
  end if
  if X is good then
    Add X to the list of good sets
  end if
  Let u ∈ R 5 such that:
    • u · (1, 1, 1, 1, 1) = 0
    • ∀v ∈ X , u · v = 0 and
    • ∀i ∈ {4, 5}, (m X ) i = 0 ⇒ u_i < 0
  V ← {v ∈ N^5 : v · u ≥ 0 and v · m_X ≤ 2}
  for every w ∈ V \ X do
    Recurse(X ∪ {w})
  end for
end procedure
```

[paper:rao]: https://arxiv.org/abs/1708.00274
[wikipedia:pentagonal_tiling]: https://en.wikipedia.org/wiki/Pentagonal_tiling
