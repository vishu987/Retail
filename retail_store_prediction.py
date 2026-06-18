# ============================================================
# Retail Store Prediction - Python Version
# Original: R (tidymodels, glm, pROC, ROCit)
# Converted by: Vishwajeet Soni
# Description: Binary classification to predict store opening
#              using Logistic Regression with cutoff optimization
# ============================================================

# ----- Required Libraries -----
import pandas as pd
import numpy as np
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import train_test_split
from sklearn.metrics import (
    roc_auc_score, roc_curve, accuracy_score,
    confusion_matrix, classification_report
)
from sklearn.preprocessing import LabelEncoder
from statsmodels.stats.outliers_influence import variance_inflation_factor
import statsmodels.api as sm
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
import warnings
warnings.filterwarnings('ignore')

# ----- 1. Load Data -----
store_train = pd.read_csv('store_train.csv')
store_test  = pd.read_csv('store_test.csv')

print("Train shape:", store_train.shape)
print("Test  shape:", store_test.shape)
print(store_train.head())
print(store_train.dtypes)

# Check target column difference
print("Columns in train but not test:",
      set(store_train.columns) - set(store_test.columns))

# ----- 2. EDA -----
# Event rate (target distribution)
print("\nEvent Rate (store):")
print(store_train['store'].value_counts(normalize=True))

print("\nStore Type distribution:")
print(store_train['store_Type'].value_counts())

# ----- Part 1: Exploratory Questions -----

# Q1: Total sales grouped by store_Type and Areaname
sales_cols = ['sales0', 'sales1', 'sales2', 'sales3', 'sales4']
store_train['total_sales'] = store_train[sales_cols].sum(axis=1)
q1 = store_train.groupby(['store_Type', 'Areaname'])['total_sales'].sum()
print("\nQ1 - Total sales by store_Type & Areaname (top 5):")
print(q1.sort_values(ascending=False).head())

# Q2: Unique store codes
print("\nQ2 - Unique storecodes:", store_train['storecode'].nunique())

# Q3: Country distribution
print("\nQ3 - Country distribution:")
print(store_train['country'].value_counts())

# Q4: Unique areas
print("\nQ4 - Unique Areanames:", store_train['Areaname'].nunique())

# Q5: Grocery Store event rate
groc = store_train[store_train['store_Type'] == 'Grocery Store']
print("\nQ5 - Grocery Store count:", len(groc), "| Positive events:", groc['store'].sum())
print("     Event rate in Grocery:", round(groc['store'].mean(), 4))

# Q6: Normality test (Kolmogorov-Smirnov equivalent)
from scipy.stats import normaltest
for s in ['sales0', 'sales1', 'sales2', 'sales3']:
    stat, p = normaltest(store_train[s].dropna())
    print(f"Q6 - {s}: stat={stat:.4f}, p-value={p:.6f} ({'NOT normal' if p < 0.05 else 'Normal'})")

# Q7: Outliers in total_sales using IQR method
q1_val = store_train['total_sales'].quantile(0.25)
q3_val = store_train['total_sales'].quantile(0.75)
iqr     = q3_val - q1_val
t_min   = q1_val - 1.5 * iqr
t_max   = q3_val + 1.5 * iqr
print(f"\nQ7 - IQR={iqr:.2f} | Lower fence={t_min:.2f} | Upper fence={t_max:.2f}")
print("     Outliers above upper:", (store_train['total_sales'] > t_max).sum())
print("     Outliers below lower:", (store_train['total_sales'] < t_min).sum())

# Visualise distribution & boxplot
fig, axes = plt.subplots(1, 2, figsize=(12, 4))
axes[0].hist(store_train['total_sales'], bins=40, color='steelblue', edgecolor='white')
axes[0].set_title('Total Sales Distribution')
axes[0].set_xlabel('Total Sales')
axes[1].boxplot(store_train['total_sales'], vert=True, patch_artist=True,
                boxprops=dict(facecolor='steelblue', color='navy'))
axes[1].set_title('Total Sales Boxplot')
plt.tight_layout()
plt.savefig('total_sales_dist.png', dpi=150)
plt.close()
print("Saved: total_sales_dist.png")

# Q8: Which store type has maximum variance in total_sales?
var_by_type = store_train.groupby('store_Type')['total_sales'].var()
print("\nQ8 - Variance in total_sales by store_Type:")
print(var_by_type)
print("Max variance store type:", var_by_type.idxmax())

# ----- Part 2: Preprocessing & Modelling -----

# Drop columns with no predictive value
drop_cols = ['Id', 'countytownname', 'Areaname']
cat_cols   = ['store_Type', 'country', 'State', 'state_alpha', 'countyname', 'storecode']
target     = 'store'

# storecode_func: keep first 5 chars
store_train['storecode'] = store_train['storecode'].astype(str).str[:5]
store_test['storecode']  = store_test['storecode'].astype(str).str[:5]

# Combine for consistent encoding
combined = pd.concat([store_train, store_test], axis=0, ignore_index=True)

# Fill missing in categorical with '__missing__'
for col in cat_cols:
    combined[col] = combined[col].fillna('__missing__').astype(str)

# Collapse rare levels (< 0.5% frequency) to '__other__'
for col in cat_cols:
    freq = combined[col].value_counts(normalize=True)
    rare = freq[freq < 0.005].index
    combined[col] = combined[col].apply(lambda x: '__other__' if x in rare else x)

# One-hot encoding
combined_dummies = pd.get_dummies(combined, columns=cat_cols, drop_first=False)

# Median imputation for numeric columns
num_cols = combined_dummies.select_dtypes(include=[np.number]).columns.tolist()
for col in num_cols:
    combined_dummies[col] = combined_dummies[col].fillna(combined_dummies[col].median())

# Split back into train & test
n_train = len(store_train)
train = combined_dummies.iloc[:n_train].copy()
test  = combined_dummies.iloc[n_train:].copy()

# Drop useless columns
train.drop(columns=[c for c in drop_cols if c in train.columns], inplace=True, errors='ignore')
test.drop(columns=[c for c in drop_cols if c in test.columns],   inplace=True, errors='ignore')

print("\nTrain after preprocessing:", train.shape)
print("Test  after preprocessing:", test.shape)

# ----- 3. Train/Validation Split (80/20) -----
np.random.seed(2)
X = train.drop(columns=[target])
y = train[target]

X_t1, X_t2, y_t1, y_t2 = train_test_split(X, y, test_size=0.2, random_state=2)
print("\nt1 (train):", X_t1.shape, "| t2 (validation):", X_t2.shape)

# ----- 4. VIF Check on Selected Features -----
selected_features = [
    'country_7', 'country_13', 'country_51', 'country_89',
    'country_91', 'country_103', 'country_123',
    'State_37', 'State_46',
    'countyname_Coos County', 'countyname_Jackson County',
    'countyname_Litchfield County', 'countyname_New Haven County',
    'countyname_Penobscot County', 'countyname_Windham County',
    'countyname_Worcester County', 'countyname_York County',
    'storecode_METRO'
]

# Normalise column names (R uses dots; Python uses spaces / underscores)
def match_cols(desired, available):
    """Fuzzy match desired feature names against available columns."""
    matched = []
    for d in desired:
        d_norm = d.replace('.', ' ').replace(' ', '_').lower()
        for a in available:
            a_norm = a.replace(' ', '_').lower()
            if d_norm == a_norm:
                matched.append(a)
                break
    return matched

avail_features = match_cols(selected_features, X_t1.columns.tolist())
print("\nMatched features for model:", avail_features)

if len(avail_features) > 0:
    X_vif = X_t1[avail_features].copy()
    X_vif_const = sm.add_constant(X_vif)
    vif_data = pd.DataFrame()
    vif_data['Feature'] = X_vif_const.columns
    vif_data['VIF']     = [variance_inflation_factor(X_vif_const.values, i)
                            for i in range(X_vif_const.shape[1])]
    print("\nVIF (top 5 highest):")
    print(vif_data.sort_values('VIF', ascending=False).head())

# ----- 5. Logistic Regression -----
if len(avail_features) > 0:
    X_train_sel = X_t1[avail_features]
    X_val_sel   = X_t2[avail_features]
    X_full_sel  = X[avail_features]
    X_test_sel  = test[avail_features] if all(f in test.columns for f in avail_features) \
                  else test[[c for c in avail_features if c in test.columns]]
else:
    # Fallback: use all numeric features
    avail_features = X_t1.select_dtypes(include=[np.number]).columns.tolist()
    X_train_sel = X_t1[avail_features]
    X_val_sel   = X_t2[avail_features]
    X_full_sel  = X[avail_features]
    X_test_sel  = test[avail_features]

# Fit on t1
lr_model = LogisticRegression(max_iter=1000, solver='lbfgs', random_state=42)
lr_model.fit(X_train_sel, y_t1)

# Validation AUC
val_scores = lr_model.predict_proba(X_val_sel)[:, 1]
tr_scores  = lr_model.predict_proba(X_train_sel)[:, 1]

val_auc = roc_auc_score(y_t2, val_scores)
tr_auc  = roc_auc_score(y_t1, tr_scores)
print(f"\nValidation AUC (t2): {val_auc:.4f}")
print(f"Training   AUC (t1): {tr_auc:.4f}")

# ----- 6. Final Model on Full Train Data -----
final_model = LogisticRegression(max_iter=1000, solver='lbfgs', random_state=42)
final_model.fit(X_full_sel, y)

train_scores = final_model.predict_proba(X_full_sel)[:, 1]

# ----- 7. Cutoff Optimisation (KS Statistic) -----
fpr, tpr, thresholds = roc_curve(y, train_scores)
ks_values  = tpr - fpr
best_idx   = np.argmax(ks_values)
my_cutoff  = thresholds[best_idx]
print(f"\nOptimal Cutoff (KS): {my_cutoff:.4f}")
print(f"KS Statistic at cutoff: {ks_values[best_idx]:.4f}")

# Build cutoff performance table
cutoff_list = []
for thresh in np.arange(0.01, 1.0, 0.01):
    preds = (train_scores >= thresh).astype(int)
    tn, fp, fn, tp = confusion_matrix(y, preds, labels=[0, 1]).ravel()
    P = tp + fn
    N = tn + fp
    acc  = (tp + tn) / (P + N) if (P + N) > 0 else 0
    sens = tp / P if P > 0 else 0
    spec = tn / N if N > 0 else 0
    prec = tp / (tp + fp) if (tp + fp) > 0 else 0
    f1   = (2 * prec * sens / (prec + sens)) if (prec + sens) > 0 else 0
    ks   = sens - (1 - spec)
    depth = (tp + fp) / (P + N) if (P + N) > 0 else 0
    cutoff_list.append({
        'Cutoff': round(thresh, 2), 'TP': tp, 'TN': tn, 'FP': fp, 'FN': fn,
        'Depth': round(depth, 4), 'Accuracy': round(acc, 4),
        'Sensitivity': round(sens, 4), 'Specificity': round(spec, 4),
        'F1': round(f1, 4), 'KS': round(ks, 4)
    })

cutoff_df = pd.DataFrame(cutoff_list).dropna().sort_values('Cutoff')
print("\nCutoff Performance Table (sample):")
print(cutoff_df[['Cutoff','Accuracy','Sensitivity','Specificity','F1','KS']].head(20).to_string(index=False))

# ----- 8. Visualisations -----

# (a) KS plot across cutoffs
fig, ax = plt.subplots(figsize=(9, 5))
ax.plot(cutoff_df['Cutoff'], cutoff_df['KS'], color='#01696f', linewidth=2, label='KS')
ax.axvline(x=my_cutoff, color='#964219', linestyle='--', label=f'Optimal cutoff = {my_cutoff:.3f}')
ax.set_title('KS Statistic Across Cutoffs', fontsize=14, fontweight='bold')
ax.set_xlabel('Cutoff')
ax.set_ylabel('KS')
ax.legend()
ax.grid(True, alpha=0.3)
plt.tight_layout()
plt.savefig('ks_plot.png', dpi=150)
plt.close()
print("Saved: ks_plot.png")

# (b) All metrics across cutoffs
fig, ax = plt.subplots(figsize=(11, 6))
colors = {'Accuracy': '#01696f', 'Sensitivity': '#da7101',
          'Specificity': '#006494', 'F1': '#7a39bb', 'KS': '#a12c7b'}
for metric, col in colors.items():
    ax.plot(cutoff_df['Cutoff'], cutoff_df[metric], label=metric, color=col, linewidth=1.8)
ax.axvline(x=my_cutoff, color='gray', linestyle='--', alpha=0.7, label=f'Cutoff={my_cutoff:.3f}')
ax.set_title('Model Performance Metrics vs Cutoff', fontsize=14, fontweight='bold')
ax.set_xlabel('Cutoff')
ax.set_ylabel('Value')
ax.legend(loc='lower left')
ax.grid(True, alpha=0.3)
plt.tight_layout()
plt.savefig('metrics_plot.png', dpi=150)
plt.close()
print("Saved: metrics_plot.png")

# (c) ROC Curve
fig, ax = plt.subplots(figsize=(7, 6))
ax.plot(fpr, tpr, color='#01696f', linewidth=2,
        label=f'ROC (AUC = {roc_auc_score(y, train_scores):.4f})')
ax.plot([0, 1], [0, 1], 'k--', alpha=0.4)
ax.scatter(fpr[best_idx], tpr[best_idx], color='#964219', zorder=5,
           label=f'Optimal cutoff = {my_cutoff:.3f}')
ax.set_title('ROC Curve', fontsize=14, fontweight='bold')
ax.set_xlabel('False Positive Rate')
ax.set_ylabel('True Positive Rate')
ax.legend()
ax.grid(True, alpha=0.3)
plt.tight_layout()
plt.savefig('roc_curve.png', dpi=150)
plt.close()
print("Saved: roc_curve.png")

# (d) Gains / Lift Table (10 groups)
def gains_table(scores, labels, ngroup=10):
    df = pd.DataFrame({'score': scores, 'label': labels})
    df = df.sort_values('score', ascending=False).reset_index(drop=True)
    df['decile'] = pd.qcut(df.index, ngroup, labels=False) + 1
    total_pos = labels.sum()
    result = []
    for d in range(1, ngroup + 1):
        grp = df[df['decile'] == d]
        result.append({
            'Decile': d,
            'Total': len(grp),
            'Events': grp['label'].sum(),
            'Event Rate': round(grp['label'].mean(), 4),
            'Cumulative Events': df[df['decile'] <= d]['label'].sum(),
            'Lift': round(grp['label'].mean() / labels.mean(), 4)
        })
    return pd.DataFrame(result)

gt = gains_table(train_scores, y)
print("\nGains Table (10 Deciles):")
print(gt.to_string(index=False))

# Lift chart
fig, ax = plt.subplots(figsize=(9, 5))
ax.bar(gt['Decile'], gt['Lift'], color='#01696f', edgecolor='white', width=0.7)
ax.axhline(y=1.0, color='#964219', linestyle='--', label='Baseline Lift = 1.0')
ax.set_title('Lift Chart by Decile', fontsize=14, fontweight='bold')
ax.set_xlabel('Decile')
ax.set_ylabel('Lift')
ax.xaxis.set_major_locator(ticker.MultipleLocator(1))
ax.legend()
ax.grid(True, alpha=0.3, axis='y')
plt.tight_layout()
plt.savefig('lift_chart.png', dpi=150)
plt.close()
print("Saved: lift_chart.png")

# ----- 9. Predict on Test Set & Save Submission -----
if all(f in test.columns for f in avail_features):
    X_test_final = test[avail_features]
else:
    X_test_final = test[[c for c in avail_features if c in test.columns]]

test_prob_scores = final_model.predict_proba(X_test_final)[:, 1]
tp_df = pd.DataFrame({'store': test_prob_scores})
tp_df.to_csv('Vishwajeet_Soni_P2_Part2again.csv', index=False)
print("\nProbability scores saved to: Vishwajeet_Soni_P2_Part2again.csv")

test_predicted = (test_prob_scores > my_cutoff).astype(int)
pd.DataFrame({'store': test_predicted}).to_csv('submission_predicted.csv', index=False)
print("Hard predictions saved to: submission_predicted.csv")

# ----- 10. Final Model Summary -----
print("\n===== FINAL MODEL SUMMARY =====")
y_pred_train = (train_scores > my_cutoff).astype(int)
print(classification_report(y, y_pred_train, target_names=['Not Open', 'Open']))
print(f"Train AUC  : {roc_auc_score(y, train_scores):.4f}")
print(f"Optimal KS cutoff: {my_cutoff:.4f}")
print(f"Predicted stores to open in test: {test_predicted.sum()}")
