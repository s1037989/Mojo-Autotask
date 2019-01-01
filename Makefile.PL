use strict;
use warnings;
 
use ExtUtils::MakeMaker;
 
WriteMakefile(
        NAME          => 'Mojo::Autotask',
        AUTHOR        => [q{Stefan Adams <stefan@adams.fm>}],
        VERSION_FROM  => 'lib/Mojo/Autotask.pm',
        ABSTRACT_FROM => 'lib/Mojo/Autotask.pm',
        PL_FILES      => {},
        PREREQ_PM     => {
                'Mojolicius' => 0,
                'Memory::Usage' => 0,
                'Devel::Size' => 0,
                'SOAP::Lite' => 0,
                'XML::LibXML' => 0,
                'Crypt::SSLeay' => 0,
                'MIME::Base64' => 0,
                'MIME::Lite' => 0,
                'XML::LibXML' => 0,
                'File::Find::Rule' => 0,
                'Test::Class' => 0,
                'Test::MockObject' => 0
        },
        dist          => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
        clean         => { FILES => 'Mojo-Autotask-*' },
);