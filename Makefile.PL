use ExtUtils::MakeMaker;

require 5.005;

WriteMakefile(
    dist	=> {
	            PREOP	=> 'pod2text FCP.pm | tee README >$(DISTVNAME)/README; chmod -R u=rwX,go=rX . ;',
	            COMPRESS	=> 'gzip -9v',
	            SUFFIX	=> '.gz',
	           },
    NAME => "Net::FCP",
    VERSION_FROM => "FCP.pm",
    'PM'	=> {
    		    'FCP.pm'		=> '$(INST_LIBDIR)/FCP.pm',
    		    'Event/Event.pm'	=> '$(INST_LIBDIR)/FCP/Event/Event.pm',
    		   },
);

