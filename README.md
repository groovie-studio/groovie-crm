# Groovie CRM

**Groovie CRM** is a lightweight customer analytics and segmentation engine built in R.
It focuses on turning raw customer data into actionable audiences and simple decision signals.

This project uses `renv` for package dependency management.

## Restore environment

Run:

```r
renv::restore()

## 🎯 Purpose

This project is designed to:

* Build a clean **customer-level dataset**
* Create simple but effective **segments**
* Detect basic **churn signals**
* Export **ready-to-use audiences** for CRM tools

The goal is clarity over complexity.

---

## ⚙️ Structure

```
groovie-crm/
│
├── data/
│   ├── raw/        # original data
│   ├── clean/      # cleaned datasets
│   └── output/     # final exports (audiences, tables)
│
├── scripts/
│   ├── 01_load_data.R
│   ├── 02_clean_data.R
│   ├── 03_customer_table.R
│   ├── 04_segmentation.R
│   ├── 05_churn_flags.R
│   └── 06_export.R
│
├── notebooks/
│   └── crm_analysis.Rmd
│
└── reports/
```

---

## 🔄 Workflow

1. Load raw data
2. Clean and standardize
3. Build customer-level table
4. Generate segments
5. Flag churn signals
6. Export CRM-ready outputs

Each step is modular and easy to modify.

---

## 🧠 Philosophy

* Keep it simple
* Avoid overengineering
* Focus on business impact
* Build fast, iterate later

---

## 🚀 Getting Started

Run scripts in order:

```r
source("scripts/01_load_data.R")
source("scripts/02_clean_data.R")
source("scripts/03_customer_table.R")
source("scripts/04_segmentation.R")
source("scripts/05_churn_flags.R")
source("scripts/06_export.R")
```

---

## 📦 Output

The system produces:

* Customer segments
* Churn flags
* CRM-ready audience files

All outputs are saved under:

```
data/output/
```

---

## 🛠 Tech

* R
* dplyr / tidyverse
* (optional) DuckDB

---

## 📌 Status

Early-stage build.
Designed for rapid iteration and real-world testing.

---

## Groovie Studio London

Independent music & data studio.
Building simple systems that actually work.
