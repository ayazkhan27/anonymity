# SGW V3+ — Mathematical Appendix

This appendix formalizes the mathematical transformations introduced in SGW V3+ to resolve structural under-identifiability on short-text domains (e.g., GoEmotions), CCC collapse (scale miscalibration), and negation leakage.

## 1. Energy-Preserving Normalization

**Problem:** Standard mean normalization (`sum(E) / length(E)`) severely attenuates score amplitude, leading to a Concordance Correlation Coefficient (CCC) collapse.

**Solution:** The model applies a sub-linear scaling factor based on the emotion token count $|\mathcal{E}|$, formulated as:

$$
\text{Normalization Factor} = \frac{1}{\sqrt{1 + |\mathcal{E}|}}
$$

This acts as an energy-preserving denominator. It prevents long strings of highly concentrated emotion words from saturating to $\pm 1$ instantaneously, while preventing the amplitude attenuation inherent to absolute division.

## 2. Identified Variance and Valence Normalization

**Problem:** Unsupervised term valences derived from embedding cosine similarity $\cos(\mathbf{w}, \mathbf{s})$ exhibit restricted variance, meaning the structural geometry (decay and dilution) exerts minimal leverage on the final score.

**Solution:** The base embedding valences are normalized prior to usage:

$$
V_i^\star = \frac{\cos(\mathbf{w}_i, \mathbf{s})}{\sqrt{\sigma_V^2}}
$$

where $\sigma_V^2$ is the global variance of all vocabulary embeddings projected onto the sentiment axis at model initialization.

## 3. Magnitude-Aware Negation

**Problem:** Pure axis inversion for negation maps "not happy" ($\cos \approx +0.6$) to $-0.6$. On weak parsers or domains where positive anchors are intrinsically misaligned, negation could inadvertently leak positive sentiment.

**Solution:** A magnitude-aware affine inversion guarantees that *any negated positive term is net negative*:

$$
N_i = \begin{cases} 
-\gamma - \delta |V_i^\star| & \text{if token } i \text{ is negated} \\
1 & \text{otherwise}
\end{cases}
$$

For structural constants, the model uses $\gamma = 0.6$ and $\delta = 0.4$.

## 4. Discourse-Aware Bidirectional Decay

**Problem:** Pure exponential positional decay suppresses the end of sequences. In complex discourse (e.g., "The food was terrible, but the service was wonderful"), the recency effect of the contrastive pivot structurally overrides primacy.

**Solution:** Detect $p = \max \{i : t_i \in \{\text{but, however, although, though}\}\}$. If a pivot exists, apply piecewise decay:

$$
w_i = \begin{cases}
\eta e^{-\lambda (i/L)} & \text{if } i < p \\
(1-\eta) e^{-\lambda (1 - i/L)} & \text{if } i \ge p
\end{cases}
$$

where $\eta < 0.5$ (default $0.4$) enforces recency supremacy post-pivot. If no pivot exists, the decay collapses to the standard $e^{-\lambda (i/L)}$.

## 5. Emotion-Conditioned Dilution

**Problem:** A text with intense emotional density theoretically requires grammatical "glue" (buffer words), unfairly penalizing highly descriptive sentiment due to structural ratio thresholds.

**Solution:** Calculate the unadjusted buffer ratio $B = \text{buffer\_count} / L$, then condition on the absolute affective mass $A = \sum |V_i^\star|$:

$$
B^\star = \frac{B}{1 + A}
$$

Dilution is then $h(D) = e^{-k B^\star}$.

## 6. Variance-Matching Calibration

**Problem:** Raw unsupervised SGW scores track monotonic scales well (high Spearman $\rho$), but miss the true ordinal spread of labeled data (low MAE / CCC). 

**Solution:** Predict scores unconditionally over the validation split to compute $\mu_S, \sigma_S$, and retrieve the ground-truth standard deviation $\sigma_Y$. A post-hoc affine transform calibrates the test scores:

$$
S' = \frac{S - \mu_S}{\sigma_S} \cdot \sigma_Y
$$

Because this is a strict affine shift, rank-correlation coefficients ($\rho, \tau$) remain invariant while MAE and CCC mathematically snap to the targeted geometric shape.

---

### Authoritative Scoring Equation

The consolidated combination of the preceding theorems defines the single point of truth for SGW V3+ inference calculation:

$$
\mathrm{SGW}(D) = \mathrm{clip}\left( \frac{ \sum_{i \in \mathcal{E}} \left[ V_i^\star \cdot e^{\alpha_i} \cdot N_i \cdot w_i \right] }{\sqrt{1 + |\mathcal{E}|}} \cdot e^{-k B^\star} , -1, 1 \right)
$$
