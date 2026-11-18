# Climate Change, Agricultural Productivity and Economic Outcomes
### Statistical Analysis and Regression Modelling in R

## 1. Research Overview

This project investigates the relationships between climate variability, agricultural productivity, and economic performance through an applied statistical analysis of multi-country agricultural data covering the period 1990–2024.

The study examines whether climatic conditions and agricultural management practices are statistically associated with crop productivity, and evaluates the extent to which agricultural output and extreme weather events relate to economic outcomes.

Rather than relying exclusively on descriptive comparisons, the analysis applies statistical inference, regression modelling, model diagnostics, and information-criterion-based model selection to evaluate competing explanations.

The primary objective is to identify meaningful statistical relationships, assess model performance, and interpret the implications of the findings within the context of agricultural and environmental sustainability.

## 2. Research Questions

The analysis addresses the following questions:

- To what extent are temperature, precipitation, and CO₂ emissions associated with agricultural productivity?
- Do agricultural management practices explain variations in crop yield?
- How strongly is agricultural productivity associated with economic outcomes?
- Are extreme weather event intensity and frequency equally informative when explaining economic impact?
- Which regression specifications provide the most appropriate statistical fit?
- Do observed agricultural and economic patterns vary across geographical regions?

## 3. Analytical Framework

The project follows a structured statistical workflow.

### 3.1 Exploratory Data Analysis

An initial investigation of the dataset was conducted to understand its statistical properties and identify patterns requiring further investigation.

Methods included:

- Descriptive statistical analysis.
- Measures of central tendency and dispersion.
- Distribution analysis using histograms and density plots.
- Skewness and kurtosis assessment.
- Boxplots and outlier investigation.
- Correlation matrices and heatmaps.

### 3.2 Statistical Inference and Hypothesis Testing

Statistical tests were implemented to evaluate relationships and differences between variables and groups.

Methods included:

- Pearson correlation tests.
- Analysis of Variance (ANOVA).
- Chi-square tests of independence.
- One-sample Z-tests.
- Statistical significance assessment using p-values.
- Effect size interpretation.

### 3.3 Regression Modelling

Several regression specifications were developed to investigate the relationships between climatic conditions, agricultural productivity, and economic outcomes.

**Linear and multiple regression**
- Simple linear regression.
- Multiple linear regression.
- Climate-based productivity modelling.
- Agricultural management-based productivity modelling.
- Combined regression specifications.

**Non-linear modelling**
- Second-degree polynomial regression.
- Investigation of potential non-linear temperature–yield relationships.
- Evaluation of precipitation and CO₂ effects.

**Model selection**
- Stepwise regression.
- Akaike Information Criterion (AIC).
- Bayesian Information Criterion (BIC).
- Log-likelihood comparison.
- Adjusted R-squared evaluation.

### 3.4 Extreme Weather and Economic Risk Modelling

A separate modelling framework was developed to investigate the economic consequences of extreme weather events.

The analysis distinguishes between:

- Event frequency: the number of extreme weather events.
- Event intensity: economic impact per recorded event.

Regression specifications were compared to investigate the relative explanatory contribution of event frequency, crop yield, and temperature.

### 3.5 Model Diagnostics

Regression diagnostics were used to assess the statistical properties of the fitted models.

The evaluation included:

- Residual distribution analysis.
- Heteroscedasticity assessment.
- Durbin–Watson testing.
- Model fit and explanatory power.
- Information-criterion-based model comparison.

## 4. Quantitative Results

The following results summarize the principal statistical findings.

### 4.1 Climate Variables and Agricultural Productivity

| Relationship / Model | Statistical Result |
|---|---:|
| Temperature and crop yield | r = 0.264 |
| Temperature–yield p-value | 0.27 |
| Precipitation and crop yield | r = 0.030 |
| CO₂ emissions and crop yield | r = -0.090 |
| Soil health and crop yield | r = -0.006 |
| Irrigation access and crop yield | r ≈ 0.000 |

The individual correlations indicate relatively weak linear relationships between the examined climate and management indicators and crop yield.

### 4.2 Regression Model Comparison

Three principal regression specifications were evaluated.

| Model | Explanatory Variables | Reported Fit |
|---|---|---:|
| Climate model | Temperature, precipitation, CO₂ emissions | R² ≈ 0.078 |
| Agricultural practices model | Irrigation, fertilizer, pesticides, soil health | R² ≈ 0.00009 |
| Combined model | Climate and agricultural variables | Adjusted R² ≈ 0.0777 |

The results indicate limited explanatory power at the aggregated dataset level. Adding agricultural management indicators did not meaningfully improve the climate-based model.

Stepwise model selection identified the climate-only specification as the preferred model among the reported candidate models.

**Selected model statistics:**
- AIC: 148,540.8
- BIC: 148,576.8
- Log-likelihood: -74,264.32

### 4.3 Agricultural Productivity and Economic Performance

One of the strongest observed relationships was between crop yield and economic impact.

| Indicator | Result |
|---|---:|
| Pearson correlation | r ≈ 0.726 |
| Historical economic benchmark | $600 million |
| Observed mean economic output | $674.27 million |
| One-sample Z-test | p < 0.001 |
| Cohen's d | 1.51 |

These findings identify agricultural productivity as an important correlate of economic performance within the dataset.

### 4.4 Extreme Weather: Frequency versus Intensity

The analysis compared alternative regression specifications for economic impact per extreme weather event.

| Model | Adjusted R² |
|---|---:|
| Event frequency | 0.359 |
| Event frequency + crop yield | 0.490 |
| Event frequency + crop yield + temperature | 0.490 |

The best reported intensity specification achieved:

- AIC: 119,754.6
- BIC: 119,790.2
- Log-likelihood: -59,872.29

The results suggest that incorporating crop yield and event-related intensity measures provides a more informative analytical framework than relying on event counts alone.

### 4.5 Additional Statistical Findings

**Geographical crop distribution**

The Chi-square test identified a statistically significant association between crop type and geographical region:

- Chi-square statistic: 349.28
- p-value: 0.0198

**Agricultural adaptation strategies**

The ANOVA comparison did not identify statistically significant differences in economic outcomes across the evaluated adaptation groups:

- F-statistic: 1.841
- p-value: 0.118

## 5. Interpretation and Research Implications

The analysis highlights several important observations:

1. Individual climate indicators exhibit weak direct linear associations with crop yield in the examined dataset.
2. The climate regression model explains a relatively small proportion of agricultural productivity variation.
3. Crop yield demonstrates a considerably stronger association with economic impact than the individual climate indicators.
4. Event intensity modelling provides additional insight into economic exposure to extreme weather.
5. Geographical differences in crop distribution warrant consideration when examining agricultural patterns.

These findings emphasize the importance of evaluating environmental and economic relationships through multiple statistical perspectives rather than relying on isolated indicators.

## 6. Limitations and Critical Evaluation

The analysis identified several limitations:

- Potentially synthetic or highly normalized patterns within the dataset.
- Limited explanatory power of the productivity regression models.
- Absence of explicit spatial-dependence modelling.
- Observational associations should not be interpreted as evidence of causality.

## 7. Tools and Technologies

**Programming language:** R

**Statistical and analytical methods:**
- Exploratory Data Analysis
- Statistical inference
- Correlation analysis
- Hypothesis testing
- Linear and polynomial regression
- Stepwise model selection
- Regression diagnostics
- Economic risk modelling

**Visualization:**
- Histograms and density plots
- Boxplots
- Scatter plots
- Correlation heatmaps
- Regression visualizations

## 8. Repository Structure

```text
Climate-Change-Agriculture/
│
├── climate_change_agriculture.R
└── README.md
```

The repository focuses on the R implementation and technical documentation. The original dataset and academic report are maintained separately.


## 9. Research Contribution

This project demonstrates the application of statistical inference and regression modelling to an interdisciplinary environmental and agricultural research problem.

Its main contribution is the comparative investigation of climate-related productivity indicators, agricultural management variables, and economic outcomes, supported by statistical testing and alternative model specifications.

The work also emphasizes model evaluation and critical interpretation, recognizing that statistical significance, explanatory power, and practical relevance represent distinct dimensions of analytical evidence.

---

**Research Area:** Applied Statistics | Environmental Data Analysis | Agricultural Economics | Regression Modelling

**Implementation:** R
