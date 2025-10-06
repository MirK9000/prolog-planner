# Prolog Planner Reachability Validation

This update augments the spiral planner with comprehensive reachability validation across the placement and orientation stages.

## Added safeguards

1. **Isolated cell filtering** – cells that have no accessible neighbours are removed before the spiral placement starts, preventing obviously unreachable desks from being considered.
2. **Grid connectivity validation** – after placement we build a grid graph of all unblocked cells and ensure every placed tile can reach at least one door-adjacent cell.
3. **PASS connectivity validation** – once orientations are assigned, we build a graph of pass strips and check that each is connected to a door clearance zone while staying clear of structural obstacles.

## New artefacts

- `prolog/connectivity.pl` – shared connectivity helpers for all three validation layers.
- `prolog/connectivity_tests.plt` – unit tests covering the new helpers.
- `prolog/test_plans/*.pl` – reproducible scenarios illustrating successful, isolated, and barrier layouts.

Run the unit tests with:

```bash
swipl -g run_tests -t halt prolog/connectivity_tests.plt
```
