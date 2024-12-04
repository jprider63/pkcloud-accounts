
# Developer Setup

This web application uses PostgreSQL as its database.
To run this web application, first install PostgreSQL and then create your database with the following commands in `psql`.

```
CREATE USER yourusername WITH PASSWORD 'YOURPASSWORD';
CREATE DATABASE yourdatabasename OWNER yourusername ENCODING 'UTF8';
\c yourdatabasename yourusername
REVOKE ALL ON DATABASE yourdatabasename FROM public;
SET timezone='UTC';
```

Update the database settings in `config/settings.yml`.
Now you should be able to launch a developer version of the web application by running:
```
stack exec -- yesod devel
```
The web application should now be running at [http://localhost:3000](http://localhost:3000).

License
=======

All code is licensed under the [MPLv2 License](LICENSE).
