
Before start, you have to prepare directory with templates. Directory is available in git repositry * /data/templates * Copy this dir in your currnet executing dir, or * ~/.agda-gpt-experiment * witch one you need to create first.

Example: age -a=Test.agda -t=not : Bool → Bool -c=myConfig.json -m=Pretty -l=15

age [OPTIONS]

Common flags: -a --agda=FILE This flag has no default value. Enter the file name of agda or the entire filepath, eg. Foo.agda -t --task=SIGNATURE This flag has no default value. Enter the function type, eg. not : Bool → Bool -c --conf=FILE this is a config file, it should be in the current directory or * ~/.agda-gpt-experiment * default value for this flag is config.json -m --mode=MODE Choose one of the operating modes. * Pretty * or * Debug * - which has more details. The default value for this flag is Pretty. -l --maxt=NUMBER Set this flag to specify the number of round conversations with ChatGPT. This flag has a default value of 5. -? --help Display help message -V --version Print version information
