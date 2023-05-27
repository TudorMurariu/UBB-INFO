from sklearn.metrics import accuracy_score, precision_score, recall_score, confusion_matrix
import math
import sklearn.metrics as sk_metrics


def classification_using_libs(real, computed, label_names):
    cm = confusion_matrix(real, computed, labels=label_names)
    acc = accuracy_score(real, computed)
    precision = precision_score(real, computed, average=None, labels=label_names)
    recall = recall_score(real, computed, average=None, labels=label_names)
    return cm, acc, precision, recall


def my_classification(real, computed, label_names):
    acc = sum([1 if real[i] == computed[i] else 0 for i in range(0, len(real))]) / len(real)
    tp = {}
    fp = {}
    tn = {}
    fn = {}
    for label in label_names:
        tp[label] = sum(
            [1 if (real[i] == label and computed[i] == label) else 0 for i in range(len(real))])
        fp[label] = sum(
            [1 if (real[i] != label and computed[i] == label) else 0 for i in range(len(real))])
        tn[label] = sum(
            [1 if (real[i] != label and computed[i] != label) else 0 for i in range(len(real))])
        fn[label] = sum(
            [1 if (real[i] == label and computed[i] != label) else 0 for i in range(len(real))])
    precision = {}
    recall = {}
    for label in label_names:
        precision[label] = tp[label] / (tp[label] + fp[label])
        recall[label] = tp[label] / (tp[label] + fn[label])
    return acc, precision, recall


def binary_classification_loss(real, computed, positive):
    actual = [[1, 0] if label == positive else [0, 1] for label in real]
    no_classes = 2
    log_loss = 0.0
    for i in range(len(real)):
        sums = - sum([actual[i][j] * math.log(computed[i][j]) + (1 - actual[i][j]) * math.log(1 - computed[i][j]) for j in range(no_classes)])
        log_loss += sums
    log_loss = log_loss / len(real)
    return log_loss


def multiclass_classification_loss(real, computed):
    # categorical cross-entropy
    no_classes = len(real)
    log_loss = 0.0
    log_loss = - sum([real[i] * math.log(computed[i]) for i in range(no_classes)])
    return log_loss


def multilabel_classification_loss(target_values, raw_outputs):
    probabilities = [1 / math.exp(-value) for value in raw_outputs]
    loss = - sum([target_values[j] * math.log(probabilities[j]) for j in range(len(target_values))])
    return loss
