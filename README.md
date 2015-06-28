# RunscopeExport
A little utility to export requests from https://www.runscope.com as separate json files.

This is my first non-toy haskell application (i.e. not solving some Project Euler problem or something).  Writing it was incredibly enlightening, but the code I'm sure is missing out on a lot of awesome Haskell idioms.  I'm looking for any feedback, feel free to create issues / pull requests with feedback.

This is still very much a work in progress:
 - Ugly code (but not as ugly as before!)
 - Most of the ugliness comes from mixing up Text, String, ByteString and Lazy.ByteString and having to shuttle between them
 - Tied to TFS json request format for filenames (I plan on taking an argument for which json field to use for the Map key)
 - Not parallel, 
 - doesn't use wreq sessions to pool TCP connections
Create two files, "auth_token.secret" and "bucket_key.secret" with your runscope API key and bucket respectively.
