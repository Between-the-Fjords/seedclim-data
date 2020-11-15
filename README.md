
<style>
p.caption {
  font-size: 0.8em;
}
</style>

This is the GitHub repository for the SeedClim project. Data paper:
Vandvik et al….

## PROJECT AND SITE INFORMATION

This project reports on…

## DATASETS, CODE AND ANALYSES

The raw and cleaned datasets are stored on OSF…

The data was processed and analysed using R. All code is stored on
github:…

### Download data

To download the data, the following function can be used: …

### Data sets

Describe relationships of the data sets…

<div class="figure">

<img src="ChinaDatabase.png" alt="Relationship of all data sets." width="60%" />

<p class="caption">

Relationship of all data sets.

</p>

</div>

### SITES

| Variable name                | Variable type | Variable range or levels                                                | How measured | Units/formats/treatment level coding |
| :--------------------------- | :------------ | :---------------------------------------------------------------------- | :----------- | :----------------------------------- |
| siteID                       | character     | Alrust - Vikesland                                                      | defined      | Alrust, Vikesland                    |
| siteCode                     | character     | alp1 - low4                                                             | defined      | alp1, sub2, bor4                     |
| latitude                     | numeric       | 60.5445 - 61.0866                                                       | measured     | decimal degree                       |
| longitude                    | numeric       | 5.96487 - 9.07876                                                       | measured     | decimal degree                       |
| elevation                    | integer       | 346 - 1213                                                              | measured     | m a.s.l.                             |
| annualPrecipitation\_gridded | numeric       | 596.348 - 3028.69                                                       | measured     | mm                                   |
| temperature\_level           | integer       | 1 - 3                                                                   | measured     | NA                                   |
| summerTemperature\_gridded   | numeric       | 5.86638 - 10.7775                                                       | measured     | °C                                   |
| precipitation\_level         | integer       | 1 - 4                                                                   | measured     | NA                                   |
| geology                      | character     | Diorittisk til granittisk gneis, migmatitt - Ryolitt, ryodacitt, dacitt | measured     | NA                                   |

### BLOCK

| Variable name | Variable type | Variable range or levels | How measured | Units/formats/treatment level coding |
| :------------ | :------------ | :----------------------- | :----------- | :----------------------------------- |
| blockID       | character     | Alr1 - Vik5              | defined      | Alr1, Vik5                           |
| siteID        | character     | Alrust - Vikesland       | defined      | Alrust, Vikesland                    |

### PLOT

| Variable name | Variable type | Variable range or levels | How measured | Units/formats/treatment level coding |
| :------------ | :------------ | :----------------------- | :----------- | :----------------------------------- |
| plotID        | integer       | 1 - 552                  | defined      | 1, 2, 3                              |
| blockID       | character     | Alr1 - Vik5              | defined      | Alr1, Vik5                           |
| aspect        | numeric       | 0                        | measured     | degree                               |
| slope         | numeric       | 0                        | measured     | degree                               |
