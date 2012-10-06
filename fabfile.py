import os

from fabric.api import env, local, run, cd, put, path
from fabric.decorators import runs_once


APP_NAME = 'hlogster'
TMP_DIR = '/tmp'
APP_DIR = '/opt/prezi/{}'.format(APP_NAME)

env.forward_agent = True
env.user = 'publisher'

env.roledefs = {'production': ['log1.us.prezi.private'], 'stage': [], 'local': []}


def _postfix():
    postfix = ''
    if 'USER' in os.environ:
        postfix = '-%s' % os.environ['USER']
    return postfix


def _git_commithash(rev):
    if rev is None:
        rev = 'HEAD'
    return local('git rev-parse --revs-only %s' % rev, capture=True)


def _git_snapshot(name, commithash, temp_directory=TMP_DIR):
    filename = '%s-%s%s.tar' % (name, commithash, _postfix())
    filename = os.path.join(temp_directory, filename)
    if not os.path.exists('%s.bz2' % filename):
        local('; '.join([
            'git archive --format tar --prefix %(prefix)s/ -o %(filename)s %(revision)s && bzip2 -9 %(filename)s'
                % {'revision': commithash, 'filename': filename, 'prefix': name},
            'rm -f %s' % filename]))
    return '%s.bz2' % filename


@runs_once
def hlogster(rev=None):
    commithash = _git_commithash(rev)
    tarball = _git_snapshot('hlogster', commithash)

    release_dir = os.path.join(APP_DIR, commithash)
    with cd(APP_DIR):
        put(tarball, 'release.tar.bz2')
        local('rm ' + tarball)
        run(' && '.join(['mkdir -p %s' % release_dir,
                         'tar -xjf release.tar.bz2 -C %s/ --strip-components=1' % commithash,
                         'rm release.tar.bz2']))

    with cd(release_dir):
        for cmd in ['pwd', 'cabal update',
                    'cabal install --only-dependencies',
                    'cabal configure',
                    'cabal build']:
            run(cmd)

    with cd(APP_DIR):
        run('ln -snf %s current' % commithash)
