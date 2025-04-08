# Simulate mackerel sampling

Rproject to help support development of a mackerel sample plan, aimed to optimise estimation of the proportion of eggs spawned daily, a key parameter to generate the egg index.

On an annual basis,a "true" spawning curve (logistic model) is first generated. Observations are then generated based on specified sample characteristics (number, frequency, state and end date, etc.). A logistic curve is estimated based on simulated observations, which allows to determine daily relative errors. The goal is to balance precision (relative error during the egg survey period) with logistical and financial considerations (e.g. max number of samples we could realistically obtain and treat prior to an assessment).


