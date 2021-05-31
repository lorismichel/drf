from setuptools import setup, find_packages
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
      version='1.0.0',
      description='Distributional Random Forest python wrapper',
      long_description=readme(),
      url='https://github.com/lorismichel/drf/tree/master/python-package',
      author='Domagoj Ćevid',
      author_email='cevid@stat.math.ethz.ch',
      install_requires=[
          'pandas',
          'rpy2',
          'numpy',
      ],
      classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: GNU General Public License v3 (GPLv3)",
        "Operating System :: OS Independent",
      ],
      include_package_data=True,
      packages=find_packages(),
      cmdclass={
        'install': CustomInstallCommand,
    })
