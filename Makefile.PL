use ExtUtils::MakeMaker;

require 5.008;

WriteMakefile(
    dist	=> {
	            PREOP	=> 'pod2text FCP.pm | tee README >$(DISTVNAME)/README; chmod -R u=rwX,go=rX . ;',
	            COMPRESS	=> 'gzip -9v',
	            SUFFIX	=> '.gz',
	           },
    NAME => "Net::FCP",
    VERSION_FROM => "FCP.pm",
    EXE_FILES    => [qw(bin/fmd)],
    PREREQ_PM    => {
                       Crypt::Twofish  => 0,
                       Crypt::Rijndael => 0,
                       Digest::SHA1    => 0,
                       Event           => 0.87, # or Tk, Glib, Coro
                    },
    PM           => {
    		    'FCP.pm'		=> '$(INST_LIBDIR)/FCP.pm',
    		    'FCP/Util.pm'	=> '$(INST_LIBDIR)/FCP/Util.pm',
    		    'FCP/Metadata.pm'	=> '$(INST_LIBDIR)/FCP/Metadata.pm',
    		    'FCP/Event/Event.pm'=> '$(INST_LIBDIR)/FCP/Event/Event.pm',
    		    'FCP/Event/Coro.pm'	=> '$(INST_LIBDIR)/FCP/Event/Coro.pm',
    		    'FCP/Event/Glib.pm'	=> '$(INST_LIBDIR)/FCP/Event/Glib.pm',
    		    'FCP/Event/Tk.pm'	=> '$(INST_LIBDIR)/FCP/Event/Tk.pm',
    		    'FCP/Key/CHK.pm'	=> '$(INST_LIBDIR)/FCP/Key/CHK.pm',
    		    'FCP/Key/SSK.pm'	=> '$(INST_LIBDIR)/FCP/Key/SSK.pm',
    		   },
);

