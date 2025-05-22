<p align="center"><img src="https://www.biomeris.it/wp-content/uploads/2021/09/logo-web.png" /></p>

# REDCapToCbio

Il pacchetto consente la trasformazione strutturata dei dati esportati da REDCap nel formato richiesto da cBioPortal, assicurando la piena compatibilità con lo schema dati previsto per il caricamento automatico nel portale.
Oltre ai dati clinici dei pazienti, il pacchetto permette anche di recuperare ed esportare i dati relativi ai campioni biologici associati, tramite le API di StudyLink.

## Installazione

Il seguente comando consente l'installazione del pacchetto:

```r
    install.packages("remotes")
    remotes::install_github("biomeris/REDCapToCbio")
```

## Esecuzione

Il file [CodeToRun.R](https://github.com/biomeris/REDCapToCbio/blob/main/inst/extras/CodeToRun.R) mostra un esempio di utilizzo delle principali funzioni per l'esportazione, la trasformazione dei dati e la creazione del file
