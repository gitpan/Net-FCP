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
    EXE_FILES    => [qw(bin/fmd)],
    PM           => {
    		    'FCP.pm'		=> '$(INST_LIBDIR)/FCP.pm',
    		    'Util.pm'		=> '$(INST_LIBDIR)/FCP/Util.pm',
    		    'Event/Event.pm'	=> '$(INST_LIBDIR)/FCP/Event/Event.pm',
    		    'Event/Coro.pm'	=> '$(INST_LIBDIR)/FCP/Event/Coro.pm',
    		    'Event/Glib.pm'	=> '$(INST_LIBDIR)/FCP/Event/Glib.pm',
    		    'Event/Tk.pm'	=> '$(INST_LIBDIR)/FCP/Event/Tk.pm',
    		    #'Key/CHK.pm'	=> '$(INST_LIBDIR)/FCP/Key/CHK.pm',
    		   },
);

