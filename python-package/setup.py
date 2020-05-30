from setuptools import setup
from setuptools.command.install import install

class CustomInstallCommand(install):
    def run(self):
        from rpy2.robjects.packages import importr
        import rpy2.robjects.packages as rpackages
        
        devtools = rpackages.importr('devtools')
        devtools.install_github('lorismichel/drf', subdir='r-package/drf')
        install.run(self)

def readme():
    with open('README.rst') as f:
        return f.read()

setup(name='drf',
      version='0.1',
      description='Distributional Random Forest python wrapper',
      long_description=readme(),
      classifiers=[
        'Programming Language :: Python :: 3.7',
       ],
      url='https://github.com/lorismichel/drf/tree/master/python-package',
      author='Domagoj Ćevid',
      author_email='cevid@stat.math.ethz.ch',
      install_requires=[
          'pandas',
          'rpy2',
          'numpy',
      ],
      include_package_data=True,
      packages=['drf'],
      cmdclass={
        'install': CustomInstallCommand,
    })

#from rpy2.robjects.packages import importr
#import rpy2.robjects.packages as rpackages

# Choosing a CRAN Mirror
#utils = rpackages.importr('utils')
#utils.chooseCRANmirror(ind=1)
# install packages from CRAN if not present
# packnames = ('drf')
# names_to_install = [x for x in packnames if not rpackages.isinstalled(x)]
# if len(names_to_install) > 0:
#     utils.install_packages(StrVector(names_to_install))