#!/bin/bash

usage() { cat <<HELP
kick-maker.sh: Generate a kick.sh with MOSSUserId in it
Usage:  kick-maker.sh [MossUserId]
Options:
  -h/--help  This help text
HELP
exit 0;
}

if [ $# = 0 ]; then
  usage
fi

MOSSUserID="000000000"
if [ $# = 1 ]; then
  case "$1" in
    -h)
      usage
      ;;
    --help)
      usage
      ;;
    *)
      MOSSUserID="$1"
      ;;
  esac
else
  usage
fi


cat > kick.sh <<END
#!/bin/bash

foo=\`screen -list | grep [.]mosssubmitterwebapp\`
if [ -z "\$foo" ]
then
  export MOSSUserID=$MOSSUserID
  SCRIPT_DIR=\$( cd -- "\$( dirname -- "\${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
  cd "\$SCRIPT_DIR"
  screen -d -m -S mosssubmitterwebapp stack exec -- yesod devel
fi
END
chmod u+x kick.sh
