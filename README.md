### (german version below)

# AI Personality analysis using LLMs

This repository contains the source code and data used in the master’s thesis  
**“Nutzung von KI als psychologisches Messinstrument für die Persönlichkeitsanalyse am Beispiel historischer Personen”**  
by **Bernd Schreiner (2025)**.

The scripts represent the working version used during the development of the thesis and may need to be slightly modified to produce the desired results.

---

## 🧠 Short Abstract

This thesis examines how Artificial Intelligence (AI)—specifically Large Language Models (LLMs)—can serve as psychological measurement tools for personality analysis. The goal was to assess whether LLMs can identify Big Five personality traits from texts written by historical figures. A custom AI-based instrument was developed and validated using the Pennebaker and King (1999) Essays dataset, then applied to works by Virginia Woolf, George Orwell, and Walter Benjamin. Results indicate that LLMs can generate consistent and theoretically plausible personality profiles. However, limitations arise regarding reliability, validity, and model transparency. Overall, the study contributes to bridging psychology and AI, highlighting the potential of data-driven approaches to personality assessment.

---

## 📂 Repository Structure

```

├── R/                  # R scripts for statistical analysis and visualization
│   ├── measurement_X.R # generation of histograms, densities, correlation and factor analysis
│   └── diff_X.         # difference analysis usign ANOVA and H-Tests
│   └── comparison_all_versions.R # create the complete comparison table (Table 13)
│   └── statistics_all_versions.R # create all measurements at once  
│   └── supplement_all_versions.R # create all supplements as .docx  
│   └── supplement/     # template for supplement
│   └── sources/        # R functions
│
├── Python/             # Python code for data scraping, preprocessing, and LLM interaction
│   └── database/       # database objects
│   └── prompts/        # prompt templates version 1 and 2
│   ├── processor_X.py  # processing logic
│   ├── scraper_X.py    # pdf and epub scraper
│   ├── classifier_X.py # LLM classifiers
│   └── requirements.txt
│
├── data/               # Data files in CSV format used in the analysis
│   ├── corpus_X.csv    # corpus data (meatdata without texts)
│   ├── development_aggregated_X # complete versions during development
│
└── README.md           # You are here

````

---

## ⚙️ Python Environment Setup

The Python code was developed and tested with **Python ≥ 3.10**.  
To reproduce the environment:

```bash
# 1. Create a virtual environment
python -m venv .venv

# 2. Activate the environment
# On Windows:
.venv\Scripts\activate
# On macOS/Linux:
source .venv/bin/activate

# 3. Install the required dependencies
pip install -r Python/requirements.txt
````

After setup, you can run scripts individually.

---

## 📊 R Environment

The R scripts use standard statistical and visualization libraries such as `tidyverse`, `psych`, and `ggplot2`.
To reproduce analyses, open the `.R` files in **RStudio** and run them sequentially after placing the data files in the `/data` directory.

---

## ⚠️ Disclaimer

The software and data in this repository are provided **“as is”**,
**without any warranties or guarantees**, express or implied.
Use at your own risk. The author assumes no responsibility for any damages
resulting from the use, misuse, or interpretation of this material.

---

## 🧠 Keywords

`Artificial Intelligence` · `Large Language Models` · `Psychology` · `Natural Language Processing` · `Big Five` · `Personality Analysis` · `Historical Texts`

---

## 🪪 License

read "LICENCE"


### (deutsch)

# KI Persönlichkeitsanalyse mit LLMs

Dieses Repository enthält den Quellcode und die Daten, die in der Masterarbeit  
**„Nutzung von KI als psychologisches Messinstrument für die Persönlichkeitsanalyse am Beispiel historischer Personen“**  
von **Bernd Schreiner (2025)** verwendet wurden.

Die Skripte entsprechen der "Arbeitsfassung" während der Bearbeitung der Thesis und müssen teils leicht modifiziert werden, um das erwünschte Ergebnis zu erzeugen.

---

## 🧠 Kurz-Abstract

Diese Masterarbeit untersucht, wie Künstliche Intelligenz (KI) – insbesondere Große Sprachmodelle (Large Language Models, LLMs) – als psychologisches Messinstrument zur Persönlichkeitsanalyse eingesetzt werden kann. Ziel war es zu prüfen, ob LLMs in der Lage sind, Persönlichkeitsmerkmale nach dem Big-Five-Modell aus Texten historischer Personen zu erkennen. Ein eigenes KI-basiertes Messinstrument wurde entwickelt und mithilfe des Essays-Datensatzes von Pennebaker und King (1999) validiert. Anschließend wurde es auf Werke von Virginia Woolf, George Orwell und Walter Benjamin angewendet. Die Ergebnisse zeigen, dass LLMs konsistente und theoretisch plausible Persönlichkeitsprofile erzeugen können. Es bestehen jedoch Einschränkungen hinsichtlich Reliabilität, Validität und Nachvollziehbarkeit der Modellentscheidungen. Insgesamt trägt die Arbeit zur Verbindung von Psychologie und KI bei und verdeutlicht das Potenzial datengetriebener Ansätze in der Persönlichkeitsforschung.

---

## 📂 Verzeichnisstruktur

```

├── R/                  # R-Skripte für statistische Analysen und Visualisierungen
│   ├── measurement_X.R
│   └── diff_X.R
│   └── comparison_all_versions.R # erzeuge die Vergleiche aller Versionen (Tabelle 13)
│   └── statistics_all_versions.R # erzeuge alle Statistiken und Tabellen  
│   └── supplement_all_versions.R # erzeuge alle Anhänge für ein .docx 
│   └── supplement/     # Vorlage für Anhang/Ergänzungen
│   └── sources/        # R-Funktionsbibliotheken
│
├── Python/             # Python-Code zum Scrapen, Verarbeiten und zur LLM-Interaktion
│   ├── processor_X.py  # Verarbeitung und Logik
│   ├── scraper_X.py    # PDF- und EPUB-Scraper
│   ├── classifier_X.py # LLM-Klassifikatoren
│   └── requirements.txt
│
├── data/               # Datendateien im CSV-Format, die in der Analyse verwendet wurden
│   ├── corpus_X.csv    # Korpusdaten (Metadaten ohne Texte)
│   ├── development_aggregated_X # Entwicklungsstände der Datensätze
│
└── README.md           # Diese Datei

````

---

## ⚙️ Einrichtung der Python-Umgebung

Der Python-Code wurde mit **Python ≥ 3.10** entwickelt und getestet.  
Zur Reproduktion der Umgebung:

```bash
# 1. Virtuelle Umgebung erstellen
python -m venv .venv

# 2. Umgebung aktivieren
# Unter Windows:
.venv\Scripts\activate
# Unter macOS/Linux:
source .venv/bin/activate

# 3. Benötigte Abhängigkeiten installieren
pip install -r Python/requirements.txt
````

Nach der Einrichtung können die Skripte einzeln ausgeführt werden.

---

## 📊 R-Umgebung

Die R-Skripte verwenden gängige Statistik- und Visualisierungsbibliotheken wie `tidyverse`, `psych` und `ggplot2`.
Zur Reproduktion der Analysen können die `.R`-Dateien in **RStudio** geöffnet und nacheinander ausgeführt werden, nachdem die Datendateien im Verzeichnis `/data` abgelegt wurden.

---

## ⚠️ Haftungsausschluss

Die Software und Daten in diesem Repository werden **„wie besehen“** bereitgestellt,
**ohne jegliche ausdrückliche oder stillschweigende Garantie oder Gewährleistung**.
Die Nutzung erfolgt auf eigenes Risiko. Der Autor übernimmt keine Haftung für Schäden,
die aus der Verwendung, dem Missbrauch oder der Interpretation dieses Materials entstehen.

---

## 🧠 Schlüsselbegriffe

`Künstliche Intelligenz` · `Große Sprachmodelle` · `Psychologie` · `Verarbeitung natürlicher Sprache` · `Big Five` · `Persönlichkeitsanalyse` · `Historische Texte`

---

## 🪪 Lizenz

Siehe Datei **„LICENCE“**.

