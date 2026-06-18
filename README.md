# 🛒 Retail Store Prediction — Python

> **Converted from R (tidymodels / glm) → Python (scikit-learn / statsmodels)**

Binary classification project to predict **whether a retail store will open** using logistic regression with cutoff optimisation.

---

## 📁 Project Files

| File | Description |
|---|---|
| `retail_store_prediction.py` | ✅ Full Python pipeline (EDA → Preprocessing → Modelling → Evaluation) |
| `Vishwajeet_Soni_P2_Part2.R` | Original R code (archived) |
| `store_train.csv` | Training dataset |
| `store_test.csv` | Test dataset |
| `Vishwajeet_Soni_P2_Part2again.csv` | Predicted probability scores on test set |
| `data dictionary (1).txt` | Column descriptions |

---

## 🔄 R → Python Mapping

| R | Python Equivalent |
|---|---|
| `tidyverse` / `dplyr` | `pandas`, `numpy` |
| `tidymodels` recipe pipeline | `pandas` preprocessing + `sklearn` pipeline |
| `glm(..., family='binomial')` | `sklearn.linear_model.LogisticRegression` |
| `stats::step()` AIC stepwise | Manual feature selection via VIF + domain logic |
| `pROC::auc()` | `sklearn.metrics.roc_auc_score` |
| `ROCit::measureit()` | Custom cutoff loop with `confusion_matrix` |
| `ROCit::ksplot()` | `sklearn.metrics.roc_curve` → KS = TPR − FPR |
| `ROCit::gainstable()` | Custom `gains_table()` function |
| `ggplot2` | `matplotlib` |
| `car::vif()` | `statsmodels.stats.outliers_influence.variance_inflation_factor` |

---

## 🧪 How to Run

```bash
# Install dependencies
pip install pandas numpy scikit-learn statsmodels matplotlib scipy

# Run the script
python retail_store_prediction.py
```

> 📌 Place `store_train.csv` and `store_test.csv` in the same directory.

---

## 📊 Output Files

The script generates the following charts and CSV outputs:

- `total_sales_dist.png` — Histogram & boxplot of total sales
- `ks_plot.png` — KS statistic across cutoffs
- `metrics_plot.png` — Accuracy, Sensitivity, Specificity, F1, KS vs Cutoff
- `roc_curve.png` — ROC Curve with optimal cutoff marked
- `lift_chart.png` — Lift chart by decile
- `Vishwajeet_Soni_P2_Part2again.csv` — Test set probability scores
- `submission_predicted.csv` — Binary predictions using optimal KS cutoff

---

## 🧠 Methodology

1. **EDA** — Event rate, normality tests, IQR outlier detection, variance by store type
2. **Preprocessing** — storecode truncation (first 5 chars), rare-level collapsing (< 0.5%), one-hot encoding, median imputation
3. **VIF Check** — Variance Inflation Factor for multicollinearity
4. **Logistic Regression** — Trained on 80% split, validated on 20%
5. **Cutoff Optimisation** — KS statistic maximisation to find optimal decision boundary
6. **Gains Table** — 10-decile lift analysis
7. **Submission** — Probability scores and hard binary predictions

---

## 👤 Author

**Vishwajeet Soni**  
Data & AI Professional | Delhi, India  
MA Economics, Delhi University | IIT-K Certified Data Analyst  
[GitHub](https://github.com/vishu987)
