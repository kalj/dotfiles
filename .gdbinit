set print pretty 1
set debuginfod enabled on

python

import os
import sys
sys.path.insert(0, os.path.expanduser('~/.gdbscripts/stlprinters/python'))
from libstdcxx.v6.printers import register_libstdcxx_printers
#register_libstdcxx_printers (None)

sys.path.append(os.path.expanduser('~/.gdbscripts/'))
import deal

from eigen_printers import register_eigen_printers
register_eigen_printers (None)
end

define hookpost-run
source ~/.gdbscripts/scheduler_lock
end
