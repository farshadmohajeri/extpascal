Ext JS wrapper for Object Pascal

version: 0.9.2, Beta 5 release, 13-nov-2008

ExtPascal is an Ext JS wrapper. 
ExtPascal lets you use the Ext JS from Object Pascal commands issued by the server using FastCGI or FastCGI thru CGI gateway. 
That brings the structure and strict syntax of the Object Pascal for programming the web browser.

Author: Wanderlan Santos dos Anjos. wanderlan.anjos@gmail.com

Home: http://extpascal.googlecode.com
Getting Started: http://code.google.com/p/extpascal/wiki/GettingStarted
Live demos: http://pitinnu.fazenda.df.gov.br/ExtPascal/SrvExtPascal
Forum: http://groups.google.com/group/extpascal
License: BSD, http://www.opensource.org/licenses/bsd-license.php

Changes since last release

- CHM and HTML help using Doc-O-Matic.
- Embedded WebServer option for Windows using Indy 10 by Vagner.
- Delphi Style event handlers by Vagner.
- Services support for Windows Vista by Patricio.
- Conversion from ISO-8859-1 to UTF8 upon Windows.
- On Linux sources should be UTF8.
- Fix for Response issue pointed by Rovi.
- Fixes for CGIGateway (loading forever issue) and BlockSocket on Linux/MacOS by Bee.
- New TApplication.Icon property.
- New TExtObject.JSReturn method.
- New TExtObject.Delete method.
- Updated TExtObject.Free method.
- New TFCGIThread.QueryAs methods.
- New TFCGIThread.BeforeThreadDestruction method.
- Removed some Ajax restrictions.
- Chars "_" and "|" are not used internally anymore.
- TExtThread.Language is a writeable property now.
- New ExtFixes classes and properties.
- A bunch of fixes and optimizations on Windows and Linux/MacOS platforms.
- Extensive tests (performance, memory use and reliability) upon Windows and Linux by Eneas.
- New performance Notes by Eneas.
- New SimplifiedFlow graph by Rovi