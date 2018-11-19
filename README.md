# cassandraaccess

## known dependencies
 - stack 1.7.1
 - libssl1.0-dev

## how to run

there has to be cassandra instance running on localhost on standard port 9042. The following queries should be run beforehand in cqlsh:

```sql
CREATE KEYSPACE alvis_test WITH replication = {'class': 'SimpleStrategy', 'replication_factor': 1} ;
CREATE TABLE alvis_test.stored_system_state ( id uuid PRIMARY KEY , value text );
INSERT INTO alvis_test.stored_system_state JSON '{"id":"35f32256-9217-4475-bd99-fc8c3ef225e5", "value":"test text"}' ;
INSERT INTO alvis_test.stored_system_state JSON '{"id":"a343ef97-da23-4f58-bf5c-cd9d33224e12", "value":"new value inserted"}' ;
INSERT INTO alvis_test.stored_system_state (e5639bd9-41d0-4007-b388-6633b86b2d98, 'testowe wstawinie' );
```

```bash
stack setup
stack build
stack cassandraaccess-exe
```


