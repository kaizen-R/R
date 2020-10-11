This example allows to read in a "Risk Register" in CSV (Excel exported, CSV2) format.

The risk register must have a header with the following headers (variables)

Risk_ID;Date_identified;Project_Name;Risk_Name;Risk_type;Description;Probability;Impact;Reaction;Reaction_details;Date_closed

A sample CSV file could be as follows:

Risk_ID;Date_identified;Project_Name;Risk_Name;Risk_type;Description;Probability;Impact;Reaction;Reaction_details;Date_closed
R1;2020/9/12;Migration Project1;Network Dependency;Planning;Some security tools depend on networking infrastructure that might not be ready in time for deployment;1;3;Accept;Re-plan security tools upon network risk materialisation (i.e. avoid project padding);2020/10/1
R2;2020/9/18;Migration Project1;Insufficient licenses;Cost;Licenses provisioned might not cover all users base;3;2;Mitigate;Review users for the platform expected after migration;2020/10/1
R3;2020/9/18;Migration Project1;Incompatible Network interfaces for new Appliances;Cost;New version of HW requires SFP+ at 10Gbps;3;1;Avoid;Buy new interfaces;2020/10/1
R4;2020/9/12;Migration Project1;Missing functional team member overlap;Personnel;One of the members of the project team in charge of networking cannot hand-over to other team members;1;2;Transfer;Require the consulting firm (by contract) to provide profiles and availability to cover for the position;2020/9/17
R5;2020/10/1;Migration Project1;Decommission of firewall;Scope;New SDN network will handle some of the old functional separation of segments through VLANs and switching;3;1;Accept;Decommission FW;2020/10/1

In Excel (or equivalent) it would look as follows:

