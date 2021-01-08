# Intercatch export

This directory contains code for export StoX 2-7-Reca projects to intercatch. In addition to Rstox and Reca, it depends on the package data.table and libraries under development for future StoX versions (RstoxData and RstoxFDA. See installation instruction for each package at https://github.com/StoXProject).

## Configuration
Various configurations have to be done in order to get all the information required by intercatch. Importantly metier or fleet have to be annotated. This annotation requires some assumptions and must be configured for specific data deliveries. The provided examples files are not suitable in general.

## Running
The function 'runExample' in interCatchExport.R can be used to run an intercatch export with mesh-sizes imported from logbooks. Study the documentation and the example stated there. Study the function body and the documentation for the functions used there to see how other ways of handling metierannotation can be integrated. 
