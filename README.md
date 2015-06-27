# RunscopeExport
A little utility to export requests from https://www.runscope.com as json files.

This is still very much a work in progress.
 - Very ugly code
 - Tied to TFS json request format for filenames
 - Not parallel, doesn't use wreq sessions to pool TCP connections
Create two files, "auth_token.secret" and "bucket_key.secret" with your runscope API key and bucket respectively.
