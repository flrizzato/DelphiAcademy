This project demonstrates InterBase XE7 Change Views support in FireDAC.
Before starting the project:
1) Run create.sql using InterBase ISQL to create InterBase database. By default it is
located in c:\sub.ib. You can change the database in create.sql.
2) Open IBChangeView.dpr project in RAD Studio IDE.
3) Click on the conOriginal.Params in Property Inspector and adjust connection parameters.
At first database path, if it was changed in step (1).
4) Run application.
5) Press "Open DB" button on top of the form. This will connect conOriginal/qOriginal,
then conChanges/qChanges and will start event alerter eaChanges.
6) Modify data in 1st grid (conOriginal/qOriginal) and see the changes in 2nd grid
(conChanges/qChanges). They appear there automatically by help of event alerter eaChanges.
The comboboxes above 2nd grid allows to choose different modes.
7) Buttons above 3d grid allows to bring into and control the changes in memory table 
mtRemote. "Merge using stream" check box allows to emulate remote data trnasfer.
