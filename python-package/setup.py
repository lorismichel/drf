from setuptools import setup

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
      keywords='forest distribution conditional heterogeneity',
      url='lorismichel/drf/python-package',
      author='Domagoj Ćevid',
      author_email='cevid@stat.math.ethz.ch',
      install_requires=[
          'pandas',
          'rpy2',
          'numpy',
      ],
      include_package_data=True,
      license='GPLv3',
      packages=['drf'],
      zip_safe=False)