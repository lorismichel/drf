import importlib
import sys
sys.path.append("../")
sys.path.append("../models")

import dataset, metrics, plotting, config
from models import cgan_model
from models import cvae_model
from models import maf_model
import numpy as np
import random
from sklearn.utils import resample
import xgboost as xgb


importlib.reload(dataset)
importlib.reload(metrics)
importlib.reload(plotting)
importlib.reload(config)
importlib.reload(cgan_model)

from tensorflow.python.framework.ops import disable_eager_execution
disable_eager_execution()

import os

dataset_list = ['vignette']
#dataset_list = ["atp1d", "atp7d"]
#dataset_list = ['jura','slump',' wq','enb','atp1d','atp7d','scpf','sf1','sf2','copula','wage','births1','births2','air']

for dat in dataset_list:

    dataset_config = config.DatasetConfig(scenario=dat, n_instance=1000)

    assert(dataset_config.scenario == "vignette"
           or dataset_config.scenario == "benchmark_1"
           or dataset_config.scenario == "jura"
           or dataset_config.scenario == "slump"
           or dataset_config.scenario == "wq"
           or dataset_config.scenario == "enb"
           or dataset_config.scenario == "atp1d"
           or dataset_config.scenario == "atp7d"
           or dataset_config.scenario == "scpf"
           or dataset_config.scenario == "sf1"
           or dataset_config.scenario == "sf2"
           or dataset_config.scenario == "copula"
           or dataset_config.scenario == "wage"
           or dataset_config.scenario == "births1"
           or dataset_config.scenario == "births2"
           or dataset_config.scenario == "air"
          )
    fig_dir = f"../figures/{dataset_config.scenario}"

    try:
        os.mkdir(fig_dir)
        print(f"Directory {fig_dir} created ")
    except FileExistsError:
        print(f"Directory {fig_dir} already exists replacing files in this notebook")

    exp_config = config.Config(
        model=config.ModelConfig(activation="elu", lr_gen=0.0001, dec_gen=0, lr_disc=0.001, optim_gen="Adam",
                                 optim_disc="Adam", z_input_size=1),
        training=config.TrainingConfig(n_epochs=5, batch_size=20, n_samples=500),
        dataset=dataset_config,
        run=config.RunConfig(save_fig=1)
    )

    # Set random seed
    np.random.seed(exp_config.model.random_seed)
    random.seed(exp_config.model.random_seed)

    #from tensorflow import set_random_seed
    #set_random_seed(exp_config.model.random_seed)

    # get the data
    X_train, y_train, X_test, y_test, X_valid, y_valid = dataset.get_dataset(exp_config.dataset.n_instance,
                                                              exp_config.dataset.scenario)

    from sklearn.preprocessing import StandardScaler

    scaler = StandardScaler()
    X_train_scaled = scaler.fit_transform(X_train)
    X_valid_scaled = scaler.transform(X_valid)
    X_test_scaled = scaler.transform(X_test)


    from sklearn.gaussian_process import GaussianProcessRegressor
    from sklearn.gaussian_process.kernels import RBF, ConstantKernel
    kernRBF = RBF(length_scale=1.0, length_scale_bounds=(1e-05,10000))
    kernConst = ConstantKernel()
    gp_rbf = GaussianProcessRegressor(kernel = kernRBF)
    gp_rbf.fit(X_train_scaled,y_train)
    #gp_const = GaussianProcessRegressor(kernel = kernConst)
    #gp_const.fit(X_train,y_train)
    mean_h = np.mean(y_train, axis=0)
    mean_h = np.array([mean_h])
    cov_h = np.cov((y_train.T-mean_h.T))

    from drf import drf
    import pandas as pd

    X_train_pd = pd.DataFrame(X_train_scaled)
    X_test_pd = pd.DataFrame(X_test_scaled)
    y_train_pd = pd.DataFrame(y_train)

    DRF = drf(min_node_size = 5, num_trees = 2000, splitting_rule = "FourierMMD") #those are the default values
    DRF.fit(X_train_pd, y_train_pd)
    DRF.info() #prints variable importance
    DRF_CART = drf(min_node_size = 5, num_trees = 2000, splitting_rule = "CART") #those are the default values
    DRF_CART.fit(X_train_pd, y_train_pd)
    DRF_CART.info() #prints variable importance

    #generate test data
    X_test_pd = pd.DataFrame(X_test)

    import tensorflow.keras as keras

    dropout_rate = 0.1

    # Comparable architecture to GAN
    model = keras.models.Sequential([
        keras.layers.Dense(500, activation="relu", input_shape=X_train_scaled.shape[1:]),
        keras.layers.Dropout(dropout_rate),
        keras.layers.Dense(500, activation="relu"),
        keras.layers.Dropout(dropout_rate),
        keras.layers.Dense(500, activation="relu"),
        keras.layers.Dropout(dropout_rate),
        keras.layers.Dense(100, activation="relu"),
        keras.layers.Dropout(dropout_rate),
        keras.layers.Dense(100, activation="relu"),
        keras.layers.Dropout(dropout_rate),
        keras.layers.Dense(50, activation="relu"),
        keras.layers.Dropout(dropout_rate),
        keras.layers.Dense(y_train.shape[1], activation="linear"),
    ])
    model.compile(loss="mean_squared_error", optimizer=keras.optimizers.Adam(lr=0.001, decay=0.1))

    callbacks = [keras.callbacks.EarlyStopping(patience=10)]
    history = model.fit(X_train_scaled, y_train,
                        validation_data=(X_valid_scaled, y_valid), epochs=100,
                        callbacks=callbacks)


    # CGAN
    cgan = cgan_model.CGAN(exp_config)
    d_loss_err, d_loss_true, d_loss_fake, g_loss_err, g_pred, g_true = cgan.train(X_train_scaled, y_train,
                                                                                  epochs=exp_config.training.n_epochs,
                                                                                  batch_size=exp_config.training.batch_size)

    # CVAE
    cvae = cvae_model.CVAE(X_train_scaled.shape[1], y_train.shape[1], exp_config)
    print("training")
    print(type(X_train_scaled))
    print(type(y_train))
    hist = cvae.train(X_train_scaled, y_train,
                      epochs=exp_config.training.n_epochs)

    from models import maf_model
    from models.maf_model import sampleMAF
    maf = maf_model.train_maf(X_train_scaled, y_train)

    print("trained!!!")

    #ypred_gan_test = cgan.predict(X_test_scaled)

    #plotting.plots(d_loss_err, d_loss_true, d_loss_fake, g_loss_err, g_pred, g_true, fig_dir, exp_config.run.save_fig)

    from sklearn.neighbors import KNeighborsRegressor
    from sklearn.neighbors import KernelDensity
    from random import choices

    knn = KNeighborsRegressor(int(np.sqrt(X_train_scaled.shape[0])))
    knn.fit(X_train_scaled, y_train)
    id = knn.kneighbors_graph(X_test_scaled)

    xgb_list = []
    for i in range(y_train.shape[1]):
        xg_reg = xgb.XGBRegressor(objective ='reg:squarederror', colsample_bytree = 1,eta=0.3, learning_rate = 0.1,
                max_depth = 5, alpha = 10, n_estimators = 2000)
        xg_reg.fit(X_train_scaled,y_train[:,i])
        xgb_list.append(xg_reg)

    def predictXGB(X):
        preds = np.zeros((X.shape[0], y_train.shape[1]))
        for i in range(len(xgb_list)):
            preds[:, i] = xgb_list[i].predict(X)
        return preds

    from sklearn.ensemble import RandomForestRegressor

    forest_list = []
    for i in range(y_train.shape[1]):
        rf = RandomForestRegressor(random_state=0, n_estimators=2000)
        rf.fit(X_train_scaled, y_train[:, i])
        forest_list.append(rf)


    def predictRF(X):
        preds = np.zeros((X.shape[0], y_train.shape[1]))
        for i in range(len(forest_list)):
            preds[:, i] = forest_list[i].predict(X)
        return preds
    # print(predictRF(X_test))

    nrep = 1
    gk_score_gp_rbf = []
    #gk_score_gp_const = []
    gk_score_knn = []
    gk_score_xgb = []
    gk_score_dnn = []
    gk_score_rf = []
    gk_score_drf = []
    gk_score_drf_cart = []
    gk_score_cgan = []
    gk_score_cvae = []
    gk_score_maf = []
    gk_score_kernel = []

    out_xgboost = predictXGB(X_test_scaled)
    out_dnn = model.predict(X_test_scaled)
    out_rf = predictRF(X_test_scaled)

    out_xgboost_train = predictXGB(X_train_scaled)
    out_dnn_train = model.predict(X_train_scaled)
    out_rf_train = predictRF(X_train_scaled)

    for r in range(nrep):

        _, _, ypred_gan_sample_test = cgan.sample(X_test_scaled, exp_config.training.n_samples)
        print("cgan!")
        ypred_cvae_sample_test = cvae.sample(X_test_scaled, exp_config.training.n_samples)
        print("cvae!")
        ypred_maf_sample_test = sampleMAF(maf, X_test_scaled, exp_config.training.n_samples)
        print("maf!")
        ypred_drf_sample_test = DRF.predict(newdata=X_test_pd, functional="sample", n=exp_config.training.n_samples).sample
        ypred_drf_cart_sample_test = DRF_CART.predict(newdata=X_test_pd, functional="sample",
                                                      n=exp_config.training.n_samples).sample
        ypred_gp_rbf_sample_test = gp_rbf.sample_y(X_test_scaled, exp_config.training.n_samples)
        #ypred_gp_const_sample_test = gp_const.sample_y(X_test, exp_config.training.n_samples)



        # KNN
        ypred_knn_sample_test = np.zeros((X_test.shape[0], y_train.shape[1], exp_config.training.n_samples))
        for i in range(X_test.shape[0]):
            _, indices = np.where(id[i, :].toarray() == 1)
            ypred_knn_sample_test[i, :, :] = np.transpose(
                y_train[choices(indices.tolist(), k=exp_config.training.n_samples), :], (1, 0))

        # KNN
        ypred_kernel_sample_test = np.zeros((X_test.shape[0], y_train.shape[1], exp_config.training.n_samples))
        for i in range(X_test.shape[0]):
            kern = KernelDensity(kernel='gaussian', bandwidth=metrics.medianHeuristic(X_train_scaled)).fit(X_test_scaled[[i],:])
            w = np.exp(kern.score_samples(X_train_scaled))
            w = w / np.sum(w)
            idx = np.random.choice(range(X_train_scaled.shape[0]), size=exp_config.training.n_samples, p=w)
            #print(np.transpose(y_train[idx, :], (1, 0)).shape)
            ypred_kernel_sample_test[i,:,:] = np.transpose(y_train[idx,:],(1,0))


        # XGBOOST
        ypred_xgboost_sample_test = np.zeros((X_test.shape[0], y_train.shape[1], exp_config.training.n_samples))
        for i in range(X_test.shape[0]):
             ypred_xgboost_sample_test[i, :, :] = np.transpose(out_xgboost[[i], :],(1,0)) + np.transpose(resample(y_train - out_xgboost_train, replace=True, n_samples=exp_config.training.n_samples),(1,0))

        # DNN
        ypred_dnn_sample_test = np.zeros((X_test.shape[0], y_train.shape[1], exp_config.training.n_samples))
        for i in range(X_test.shape[0]):
            ypred_dnn_sample_test[i, :, :] = np.transpose(out_dnn[[i], :],(1,0)) + np.transpose(resample(y_train - out_dnn_train, replace=True, n_samples=exp_config.training.n_samples),(1,0))

        # RF
        ypred_rf_sample_test = np.zeros((X_test.shape[0], y_train.shape[1], exp_config.training.n_samples))
        for i in range(X_test.shape[0]):
            ypred_rf_sample_test[i, :, :] = np.transpose(out_rf[[i], :],(1,0)) + np.transpose(resample(y_train - out_rf_train, replace=True, n_samples=exp_config.training.n_samples),(1,0))

        #ypred_gp_const_sample_test = np.zeros((X_test.shape[0], y_train.shape[1], exp_config.training.n_samples))
        #for i in range(X_test.shape[0]):
        #    ypred_gp_const_sample_test[i, :, :] = np.transpose(
        #        np.random.multivariate_normal(out_homo[i, :], cov_h, exp_config.training.n_samples), (1, 0))
        for k in range(X_test.shape[0]):
            print(metrics.medianHeuristic(np.transpose(ypred_knn_sample_test,(2,1,0))[:,:,k]))


        import pickle

        d = {'X_test_scaled': X_test_scaled,
             'X_test': X_test,
             'ypred_gan_sample_test': ypred_gan_sample_test,
             'ypred_cvae_sample_test': ypred_cvae_sample_test,
             'ypred_maf_sample_test': ypred_maf_sample_test,
             'ypred_drf_sample_test': ypred_drf_sample_test,
             'ypred_drf_cart_sample_test': ypred_drf_cart_sample_test,
             'ypred_dnn_sample_test': ypred_dnn_sample_test,
             'ypred_rf_sample_test': ypred_rf_sample_test,
             'ypred_xgboost_sample_test': ypred_xgboost_sample_test,
             'ypred_knn_sample_test': ypred_knn_sample_test,
             'ypred_kernel_sample_test': ypred_kernel_sample_test,
             'ypred_gp_rbf_sample_test': ypred_gp_rbf_sample_test}

        with open("vignette.data", 'wb') as vignette_data:
            pickle.dump(d, vignette_data)

        #gk_score_gp_const.append(metrics.GK_score(y_test, ypred_gp_const_sample_test, "GP CONST"))
        gk_score_gp_rbf.append(metrics.GK_score(y_test, ypred_gp_rbf_sample_test, "GP RBF"))
        gk_score_kernel.append(metrics.GK_score(y_test, ypred_kernel_sample_test, "KERNEL"))
        gk_score_knn.append(metrics.GK_score(y_test, ypred_knn_sample_test, "KNN"))
        gk_score_xgb.append(metrics.GK_score(y_test, ypred_xgboost_sample_test, "XGB"))
        gk_score_rf.append(metrics.GK_score(y_test, ypred_rf_sample_test, "RF"))
        gk_score_dnn.append(metrics.GK_score(y_test, ypred_dnn_sample_test, "DNN"))
        gk_score_drf.append(metrics.GK_score(y_test, ypred_drf_sample_test, "DRF"))
        gk_score_drf_cart.append(metrics.GK_score(y_test, ypred_drf_cart_sample_test, "DRF (CART)"))
        gk_score_cgan.append(metrics.GK_score(y_test, ypred_gan_sample_test, "CGAN"))
        gk_score_cvae.append(metrics.GK_score(y_test, ypred_cvae_sample_test, "CVAE"))
        gk_score_maf.append(metrics.GK_score(y_test, ypred_maf_sample_test, "MAF"))

    gk_rbf_score_mean = np.mean(np.array(gk_score_gp_rbf))
    gk_rbf_score_sd = np.std(np.array(gk_score_gp_rbf))

    gk_kernel_score_mean = np.mean(np.array(gk_score_kernel))
    gk_kernel_score_sd = np.std(np.array(gk_score_kernel))

    gk_drf_score_mean = np.mean(np.array(gk_score_drf))
    gk_drf_score_sd = np.std(np.array(gk_score_drf))

    gk_drf_cart_score_mean = np.mean(np.array(gk_score_drf_cart))
    gk_drf_cart_score_sd = np.std(np.array(gk_score_drf_cart))

    gk_cgan_score_mean = np.mean(np.array(gk_score_cgan))
    gk_cgan_score_sd = np.std(np.array(gk_score_cgan))

    gk_cvae_score_mean = np.mean(np.array(gk_score_cvae))
    gk_cvae_score_sd = np.std(np.array(gk_score_cvae))

    gk_maf_score_mean = np.mean(np.array(gk_score_maf))
    gk_maf_score_sd = np.std(np.array(gk_score_maf))

    gk_xgb_score_mean = np.mean(np.array(gk_score_xgb))
    gk_xgb_score_sd = np.std(np.array(gk_score_xgb))

    gk_dnn_score_mean = np.mean(np.array(gk_score_dnn))
    gk_dnn_score_sd = np.std(np.array(gk_score_dnn))

    gk_rf_score_mean = np.mean(np.array(gk_score_rf))
    gk_rf_score_sd = np.std(np.array(gk_score_rf))

    gk_knn_score_mean = np.mean(np.array(gk_score_knn))
    gk_knn_score_sd = np.std(np.array(gk_score_knn))

    if exp_config.run.save_fig:
        file = open(f"{fig_dir}/metrics.txt","w")
        file.write(f"===Test Score===\n")
        file.write(f"GP RBF score: {gk_rbf_score_mean} +- {gk_rbf_score_sd}\n")
        file.write(f"KERNEL score: {gk_kernel_score_mean} +- {gk_kernel_score_sd}\n")
        file.write(f"RF score: {gk_rf_score_mean} +- {gk_rf_score_sd}\n")
        file.write(f"XGB score: {gk_xgb_score_mean} +- {gk_xgb_score_sd}\n")
        file.write(f"DNN score: {gk_dnn_score_mean} +- {gk_dnn_score_sd}\n")
        file.write(f"KNN score: {gk_knn_score_mean} +- {gk_knn_score_sd}\n")
        file.write(f"GAN score: {gk_cgan_score_mean} +- {gk_cgan_score_sd}\n")
        file.write(f"CVAE score: {gk_cvae_score_mean} +- {gk_cvae_score_sd}\n")
        file.write(f"MAF score: {gk_maf_score_mean} +- {gk_maf_score_sd}\n")
        file.write(f"DRF score: {gk_drf_score_mean} +- {gk_drf_score_sd}\n")
        file.write(f"DRF (CART) score: {gk_drf_cart_score_mean} +- {gk_drf_cart_score_sd}\n")
        file.close()

    print(ypred_gp_rbf_sample_test)
