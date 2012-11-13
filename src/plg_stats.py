def load_lang_data(path='../data/comparison.csv') :
    import csv
    raw_data = []
    with open(path, 'rb') as csvfile :
        reader = csv.reader(csvfile, delimiter='\t')
        for line in reader :
            raw_data.append(line)

    labeled_data = []
    labels = raw_data[0]
    for line in raw_data[1:] :
        labeled_line = {}
        for i in range(0, len(labels)) :
            labeled_line[labels[i]] = line[i]
        labeled_data.append(labeled_line)

    return labeled_data


def extract_vals(data, x_key, y_key) :
    """ data should be a list of dictionaries, x_key and y_key
    are the two keys to parse with. The values will be sorted by
    x value """

    x = []
    y = []
    for vector in data :
        x.append(vector[x_key])
        y.append(vector[y_key])

    return zip(*sorted(zip(x, y)))

def doublize(sarr) :
    darr = []
    for elem in sarr :
        darr.append(float(elem))
    return darr

def plot_fit(data, x_key, y_key) :
    import matplotlib.pyplot as plt
    import scipy.stats as sst
    from scipy import optimize
    import numpy as np

    x, y = extract_vals(data, x_key, y_key)
    x = np.array(doublize(x))
    y = np.array(doublize(y))

    fitfunc = lambda p, x: p[0]*x + p[1]
    errfunc = lambda p, x, y: fitfunc(p, x) - y

    slope_guess = (y[-1] - y[0]) / (x[-1] - x[0])
    intercept_guess = (y[-1] + y[0]) / 2 - slope_guess * (x[-1] + x[0]) / 2

    p0 = [slope_guess, intercept_guess]

    p1, success = optimize.leastsq(errfunc, p0[:], args=(x, y))

    r, p = sst.pearsonr(x, y)

    plt.xlabel(x_key)
    plt.ylabel(y_key)

    plt.plot(x, y, "rx", x, fitfunc(p1, x), 'b-')
    x_coord = (x.max() - x.min()) * 3 / 4 + x.min()
    y_coord = (y.max() - y.min()) * 5 / 6 + y.min()
    plt.text(x_coord, y_coord, 'Pearson r = %.2f\nP value = %.2f' %(r, p))
    plt.show()

def anova_oneway (data, val_key, group_key, treatment_group=None) :
    """ performs a 1-way anova (equivalent to a student's t-test for
    only two groups)"""

    import scipy.stats as sst
    import numpy as np
    import pylab

    # First we must divide up the groups
    groups = {}
    for vector in data :
        if vector[group_key] not in groups.keys() :
            print 'Found new group:', vector[group_key]
            groups[vector[group_key]] = []

        groups[vector[group_key]].append(vector[val_key])

    for g in groups.keys() :
        groups[g] = np.array(doublize(groups[g]))

        print 'Group', g
        print groups[g]

    # Do the statistics
    f, p = sst.f_oneway(*groups.values())

    # Plot the graph
    labels = groups.keys()
    for l in range(0, len(labels)) :
        label = labels[l]
        pylab.plot([l]*len(groups[label]), groups[label], "x")
        mean = groups[label].mean()
        size = len(groups[label])
        pylab.plot(l, mean, "ro")
        pylab.text((l+.1), mean, 'Group "%s"\nMean %.4f\n(%d values)'
                %(label, mean, size))

    pylab.ylabel(val_key)
    pylab.xlabel(group_key)
    pylab.xticks(range(-1, len(labels)+1), [''] + labels + [''])

    # Print the statistics
    yt = pylab.yticks()[0]
    info_y = yt[0] + (yt.max() - yt.min()) * 5/6
    info_x = -.9
    pylab.text(info_x, info_y, 'F value %.2f\np value %.4f' %(f, p))

    pylab.show()

    return f, p
